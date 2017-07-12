#include "dynet/nodes.h"
#include "dynet/dynet.h"
#include "dynet/training.h"
#include "dynet/timing.h"
#include "dynet/rnn.h"
#include "dynet/gru.h"
#include "dynet/lstm.h"
#include "dynet/dict.h"
#include "dynet/expr.h"

// Platform-independent header to allow calls to getpid()
#if _WINDOWS
    #include <process.h>
#endif

#include <iostream>
#include <fstream>
#include <sstream>

#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>


#include "ExternalSpines.pb.h"

using namespace std;
using namespace dynet;

unsigned LAYERS = 2;
unsigned INPUT_DIM = 128;
unsigned HIDDEN_DIM = 256;
unsigned TAG_HIDDEN_DIM = 64;
unsigned TAG_SIZE = 0;
unsigned VOCAB_SIZE = 0;

double CUTOFF_RATIO = 1.0;

dynet::Dict d;
dynet::Dict td;
int kNONE;
int kSOS;
int kEOS;
vector<int> pruning_info;
vector<int> pruning_info_gold;
const int PRUNING_BUCKETS = 1000;
const double PRUNING_MAX = 1.0;
const double PRUNING_DELTA = -0.001;

template <class Builder>
struct BiLSTMTagger {
  LookupParameter p_w;
  Parameter p_l2th;
  Parameter p_r2th;
  Parameter p_thbias;

  Parameter p_th2t;
  Parameter p_tbias;
  Builder l2rbuilder;
  Builder r2lbuilder;
  explicit BiLSTMTagger(Model& model) :
      l2rbuilder(LAYERS, INPUT_DIM, HIDDEN_DIM, model),
      r2lbuilder(LAYERS, INPUT_DIM, HIDDEN_DIM, model) {
    p_w = model.add_lookup_parameters(VOCAB_SIZE, {INPUT_DIM}); 
    p_l2th = model.add_parameters({TAG_HIDDEN_DIM, HIDDEN_DIM});
    p_r2th = model.add_parameters({TAG_HIDDEN_DIM, HIDDEN_DIM});
    p_thbias = model.add_parameters({TAG_HIDDEN_DIM});

    p_th2t = model.add_parameters({TAG_SIZE, TAG_HIDDEN_DIM});
    p_tbias = model.add_parameters({TAG_SIZE});
  }

  // return Expression of total loss
  Expression BuildTaggingGraph(
      const vector<int>& sent, const vector<int>& tags,
      ComputationGraph& cg, double* cor = 0, unsigned* ntagged = 0,
      bool print_labels = false, bool eval = false,
      graphparser::Sentence* nsent = nullptr
  ) {
    const unsigned slen = sent.size();
    l2rbuilder.new_graph(cg);  // reset RNN builder for new graph
    l2rbuilder.start_new_sequence();
    r2lbuilder.new_graph(cg);  // reset RNN builder for new graph
    r2lbuilder.start_new_sequence();
    Expression i_l2th = parameter(cg, p_l2th);
    Expression i_r2th = parameter(cg, p_r2th);
    Expression i_thbias = parameter(cg, p_thbias);
    Expression i_th2t = parameter(cg, p_th2t);
    Expression i_tbias = parameter(cg, p_tbias); 
    vector<Expression> errs;
    vector<Expression> i_words(slen);
    vector<Expression> fwds(slen);
    vector<Expression> revs(slen);

    // set words, adding noise during training (non-eval)
    for (unsigned t = 0; t < slen; ++t) {
///      if (print_probs)
///        cout << t << " " << sent[t] << " " << d.convert(sent[t]) << endl;
      i_words[t] = lookup(cg, p_w, sent[t]);
      if (!eval) { i_words[t] = noise(i_words[t], 0.1); }
    }

    // read sequence from left to right
    l2rbuilder.add_input(lookup(cg, p_w, kSOS));
    for (unsigned t = 0; t < slen; ++t)
      fwds[t] = l2rbuilder.add_input(i_words[t]);
    // read sequence from right to left
    r2lbuilder.add_input(lookup(cg, p_w, kEOS));
    for (unsigned t = 0; t < slen; ++t)
      revs[slen - t - 1] = r2lbuilder.add_input(i_words[slen - t - 1]);

    for (unsigned t = 0; t < slen; ++t) {
      if (tags[t] != kNONE) {
        if (ntagged) (*ntagged)++;
        Expression i_th = tanh(affine_transform({i_thbias, i_l2th, fwds[t], i_r2th, revs[t]}));
        Expression i_t = affine_transform({i_tbias, i_th2t, i_th});
        if (cor) {
          vector<float> dist = as_vector(cg.incremental_forward(i_t));

          // Find best tag according to the distribution and also collect info
          // on the score for the correct tag.
          double best = -9e99;
          double worst = 9e99;
          double corr_score = 0.0;
          int besti = -1;
          if (print_labels)
            cout << "Word " << t << " " << d.convert(sent[t]);
          for (unsigned int i = 0; i < dist.size(); ++i) {
///            if (print_probs) cout << " " << dist[i];
            if (i == tags[t]) corr_score = dist[i];
            if (dist[i] > best) {
              best = dist[i];
              besti = i;
            }
            if (dist[i] < worst) worst = dist[i];
          }
          if (print_labels)
            cout << " " << besti << " correct: " << tags[t] << " ( " << td.convert(besti) << " vs. " << td.convert(tags[t]) << " )" << endl;
          if (tags[t] == besti) (*cor)++;

          if (eval) {
            // Record information for pruning
            double range = best - worst;

            // Save scores
            graphparser::ScoreSet* saved_scores = nullptr;
            if (nsent != nullptr) {
              saved_scores = nsent->add_scores();
              saved_scores->set_min_score(worst);
              double scaled_ratio = (range * CUTOFF_RATIO) + worst;
              for (unsigned int i = 0; i < dist.size(); ++i) {
                if (dist[i] > scaled_ratio) {
                  graphparser::Score* cscore = saved_scores->add_scores();
                  cscore->set_id(i);
                  cscore->set_score(dist[i]);
                }
              }
            }

            // Sort dist to make the next stage more efficient
            sort(dist.begin(), dist.end());
            int cpos = 0;
            for (unsigned int i = dist.size() - 1; i > 0; i--) {
              int count = dist.size() - i - 1;
              double scaled = (dist[i] - worst) / range;
              while (
                  cpos * PRUNING_DELTA + PRUNING_MAX > scaled &&
                  cpos < PRUNING_BUCKETS - 2
              ) {
                pruning_info[cpos] += count;
                if (dist[i] < corr_score) pruning_info_gold[cpos] += 1;
                cpos += 1;
              }
            }
            pruning_info[PRUNING_BUCKETS - 1] += dist.size();
            pruning_info_gold[PRUNING_BUCKETS - 1] += 1;
          }
        }

        Expression i_err = pickneglogsoftmax(i_t, tags[t]);
        errs.push_back(i_err);
      }
    }
    if (print_labels) cout << endl;
    return sum(errs);
  }
};

string get_string_arg(int argc, char** argv, string arg, string default_val="") {
  for (int i = 0; i < argc; i++) {
    if (arg.compare(argv[i]) == 0 && i < (argc - 1)) {
      return string(argv[i + 1]);
    }
  }
  return default_val;
}

double get_double_arg(int argc, char** argv, string arg, double default_val) {
  for (int i = 0; i < argc; i++) {
    if (arg.compare(argv[i]) == 0 && i < (argc - 1)) {
      return stod(string(argv[i + 1]));
    }
  }
  return default_val;
}

int get_int_arg(int argc, char** argv, string arg, int default_val) {
  for (int i = 0; i < argc; i++) {
    if (arg.compare(argv[i]) == 0 && i < (argc - 1)) {
      return stoi(string(argv[i + 1]));
    }
  }
  return default_val;
}

bool get_bool_arg(int argc, char** argv, string arg) {
  for (int i = 0; i < argc; i++) {
    if (arg.compare(argv[i]) == 0) return true;
  }
  return false;
}

int main(int argc, char** argv) {
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  dynet::initialize(argc, argv);
  if (argc < 3) {
    cerr << "Usage: "
      << argv[0] << endl
      << "  [-train corpus.txt]" << endl
      << "  [-dev dev.txt]" << endl
      << "  [-test test.txt]" << endl
      << "  [-model model.params]" << endl
      << "  [-word-dict dict.words]" << endl
      << "  [-tag-dict dict.tags]" << endl
      << "  [-prefix name (default=tagger)]" << endl
      << "  [-cutoff-ratio CUTOFF_RATIO]" << endl
      << "  [-layers LAYERS]" << endl
      << "  [-input-dim INPUT_DIM]" << endl
      << "  [-hidden-dim HIDDEN_DIM]" << endl
      << "  [-tag-hidden-dim TAG_HIDDEN_DIM]" << endl
      << "  [-print-labels]" << endl
      << endl;
    return 1;
  }
  bool doing_train = get_bool_arg(argc, argv, "-train");
  bool doing_test = get_bool_arg(argc, argv, "-test");
  bool min_loss = get_bool_arg(argc, argv, "-min_loss");
  string prefix = get_string_arg(argc, argv, "-prefix", "tagger");

  CUTOFF_RATIO = get_double_arg(argc, argv, "-cutoff-ratio", CUTOFF_RATIO);
  LAYERS = get_int_arg(argc, argv, "-layers", LAYERS);
  INPUT_DIM = get_int_arg(argc, argv, "-input-dim", INPUT_DIM);
  HIDDEN_DIM = get_int_arg(argc, argv, "-hidden-dim", HIDDEN_DIM);
  TAG_HIDDEN_DIM = get_int_arg(argc, argv, "-tag-hidden-dim", TAG_HIDDEN_DIM);
  bool print_labels = get_bool_arg(argc, argv, "-print-labels");

  vector<pair<vector<int>,vector<int>>> training, dev;
  vector<pair<int, pair<vector<int>,vector<int>>>> test;

  if (doing_train) {
    kNONE = td.convert("*");
    kSOS = d.convert("<s>");
    kEOS = d.convert("</s>");
    string line;
    int tlc = 0;
    int ttoks = 0;
    string train_in = get_string_arg(argc, argv, "-train");
    assert(train_in.compare("") != 0);
    cerr << "Reading training data from " << train_in << "...\n";
    {
      ifstream in(train_in);
      assert(in);
      while(getline(in, line)) {
        auto found = line.find("# SentID");
        if (found == std::string::npos) {
          ++tlc;
          int nc = 0;
          vector<int> x,y;
          read_sentence_pair(line, x, d, y, td);
          assert(x.size() == y.size());
          if (x.size() == 0) { cerr << line << endl; abort(); }
          training.push_back(make_pair(x,y));
          for (unsigned i = 0; i < y.size(); ++i) {
            if (y[i] != kNONE) { ++nc; }
          }
          if (nc == 0) {
            cerr << "No tagged tokens in line " << tlc << endl;
            abort();
          }
          ttoks += x.size();
        }
      }
      cerr << tlc << " lines, " << ttoks << " tokens, " << d.size() << " types\n";
      cerr << "Tags: " << td.size() << endl;
    }

    d.freeze(); // no new word types allowed
    td.freeze(); // no new tag types allowed
    d.set_unk("UNKNOWN_WORD");
    td.set_unk("UNKNOWN_TAG");

    ostringstream file_dw;
    ostringstream file_dt;
    file_dw << prefix << ".dict.words";
    file_dt << prefix << ".dict.tags";
    ofstream ows(file_dw.str());
    ofstream ots(file_dt.str());
    boost::archive::text_oarchive ow(ows);
    boost::archive::text_oarchive ot(ots);
    ow << d;
    ot << td;
  } else {
    string words_infile = get_string_arg(argc, argv, "-word-dict");
    string tags_infile = get_string_arg(argc, argv, "-tag-dict");
    cerr << "Reading existing word dictionary from: " << words_infile << endl;
    cerr << "Reading existing tag dictionary from: " << tags_infile << endl;
    ifstream words_in(words_infile);
    ifstream tags_in(tags_infile);
    boost::archive::text_iarchive words_in_archive(words_in);
    boost::archive::text_iarchive tags_in_archive(tags_in);
    words_in_archive >> d;
    tags_in_archive >> td;
  }

  VOCAB_SIZE = d.size();
  TAG_SIZE = td.size();

  if (! doing_test) {
    int dlc = 0;
    int dtoks = 0;
    string dev_in = get_string_arg(argc, argv, "-dev");
    assert(dev_in.compare("") != 0);
    cerr << "Reading dev data from " << dev_in << "...\n";
    {
      ifstream in(dev_in);
      assert(in);
      string line;
      while(getline(in, line)) {
        auto found = line.find("# SentID");
        if (found == std::string::npos) {
          ++dlc;
          vector<int> x,y;
          read_sentence_pair(line, x, d, y, td);
          assert(x.size() == y.size());
          dev.push_back(make_pair(x,y));
          dtoks += x.size();
        }
      }
      cerr << dlc << " lines, " << dtoks << " tokens\n";
    }
  }

  if (doing_test) {
    int tlc = 0;
    int ttoks = 0;
    int sentence_id = -1;
    string test_fname = get_string_arg(argc, argv, "-test");
    cerr << "Reading test data from " << test_fname << "...\n";
    {
      ifstream in(test_fname);
      assert(in);
      string line;
      while(getline(in, line)) {
        // Get the ID
        auto found = line.find("# SentID");
        if (found == std::string::npos) {
          ++tlc;
          vector<int> x,y;
          read_sentence_pair(line, x, d, y, td);
          assert(x.size() == y.size());
          test.push_back(make_pair(sentence_id, make_pair(x,y)));
          ttoks += x.size();
        } else {
          sentence_id = stoi(line.substr(8));
        }
      }
      cerr << tlc << " lines, " << ttoks << " tokens\n";
    }
  }


  Model model;
///  Trainer* sgd = new MomentumSGDTrainer(model);
  Trainer* sgd = new AdamTrainer(model);
  BiLSTMTagger<LSTMBuilder> tagger(model);
  string model_fname = get_string_arg(argc, argv, "-model");
  if (model_fname.compare("") != 0) {
    cerr << "Reading existing model params from: " << model_fname << endl;
    ifstream in(model_fname);
    boost::archive::text_iarchive ia(in);
    ia >> model;
  }

  for (int i = 0; i < PRUNING_BUCKETS; i++) {
    pruning_info.push_back(0);
    pruning_info_gold.push_back(0);
  }

  if (doing_test) {
    ostringstream os;
    os << prefix << ".data";
    const string fname = os.str();

    graphparser::Scores scores_for_saving;

    // First, print the tag mapping
    for (unsigned int i = 0; i < td.size(); ++i) {
      string spine_text = td.convert(i);
      graphparser::Spine* spine = scores_for_saving.add_spines();
      spine->set_id(i);
      spine->set_text(spine_text);
///      cout << "Tag " << i << " " << spine << endl;
    }

    double tloss = 0;
    unsigned ttags = 0;
    double tcorr = 0;
    for (auto& id_sent : test) {
      int sentence_id = id_sent.first;
      auto sent = id_sent.second;
      ComputationGraph cg;
      graphparser::Sentence* nsent = scores_for_saving.add_sentences();
      nsent->set_id(sentence_id);
      tagger.BuildTaggingGraph(sent.first, sent.second, cg, &tcorr, &ttags,
          print_labels, true, nsent);
    }

    // Write the new address book back to disk.
    fstream output(fname, ios::out | ios::trunc | ios::binary);
    if (!scores_for_saving.SerializeToOstream(&output)) {
      cerr << "Failed to write scores for saving." << endl;
      return -1;
    }
  } else {
    double best = 9e+99; // Tracks the loss for the best model so far
    double best_prune = 1.0; // Tracks the best pruning by a model so far

    // Prepare filename for model storage, and write dictionaries
    ostringstream os;
    os << prefix << ".params";
    const string fname = os.str();
    cerr << "Parameters will be written to: " << fname << endl;

    unsigned report_every_i = get_int_arg(argc, argv, "-report-freq", 1000);
    unsigned dev_every_i_reports = get_int_arg(argc, argv, "-dev-report-freq", 10);
    unsigned si = training.size();
    vector<unsigned> order(training.size());
    for (unsigned i = 0; i < order.size(); ++i) order[i] = i;
    bool first = true;
    int report = 0;
    unsigned lines = 0;
    while(1) {
      Timer iteration("completed in");
      double loss = 0;
      unsigned ttags = 0;
      double correct = 0;
      for (unsigned i = 0; i < report_every_i; ++i) {
        if (si == training.size()) {
          si = 0;
          if (first) { first = false; } else { sgd->update_epoch(); }
          cerr << "**SHUFFLE\n";
          shuffle(order.begin(), order.end(), *dynet::rndeng);
        }

        // build graph for this instance
        ComputationGraph cg;
        auto& sent = training[order[si]];
        ++si;
        Expression loss_expr = tagger.BuildTaggingGraph(sent.first, sent.second, cg, &correct, &ttags, false, false);

        // Run forward pass, backpropagate, and do an update
        loss += as_scalar(cg.forward(loss_expr));
        cg.backward(loss_expr);
        sgd->update(1.0);
        ++lines;
      }
      sgd->status();
      cerr << " E = " << (loss / ttags) << " ppl=" << exp(loss / ttags) << " (acc=" << (correct / ttags) << ") ";

      // show score on dev data?
      report++;
      if (report % dev_every_i_reports == 0) {

        // Prepare storage for pruning information
        pruning_info.clear();
        pruning_info_gold.clear();
        for (int i = 0; i < PRUNING_BUCKETS; i++) {
          pruning_info.push_back(0);
          pruning_info_gold.push_back(0);
        }

        double dloss = 0;
        unsigned dtags = 0;
        double dcorr = 0;
        for (auto& sent : dev) {
          ComputationGraph cg;
          Expression loss_expr = tagger.BuildTaggingGraph(sent.first, sent.second, cg, &dcorr, &dtags, false, true);
          dloss += as_scalar(cg.forward(loss_expr));
        }
        // Return pruning information
        double goal = 0.97;
        double gold_tag_count = pruning_info_gold.back();
        double tag_count = pruning_info.back();
        double score_at_99 = 1.0;
        for (int i = 0; i < PRUNING_BUCKETS; i++) {
          // Identify if the goal is met
          if (goal * gold_tag_count < pruning_info_gold[i]) {
            double gold_pruned = pruning_info_gold[i] / gold_tag_count;
            double pruned = pruning_info[i] / tag_count;
            if ((goal - 0.99) < 0.001) score_at_99 = pruned;
            cerr << "\n***DEV Pruning " << gold_pruned << " gold, " << pruned << " = " << pruning_info[i] << " at " << (PRUNING_MAX + i * PRUNING_DELTA);
            goal += 0.005;
          }
          // Note the result at 99 (to be used to determine model saving)
        }
        if (min_loss) {
          if (dloss < best) {
            best = dloss;
            ofstream out(fname);
            boost::archive::text_oarchive oa(out);
            oa << model;
          }
        } else {
          if (score_at_99 < best_prune) {
            best_prune = score_at_99;
            ofstream out(fname);
            boost::archive::text_oarchive oa(out);
            oa << model;
          }
        }

        cerr << "\n***DEV [epoch=" << (lines / (double)training.size()) << "] E = " << (dloss / dtags) << " ppl=" << exp(dloss / dtags) << " acc=" << (dcorr / dtags) << ' ';
      }
    }
  }

  google::protobuf::ShutdownProtobufLibrary();

  delete sgd;
}

