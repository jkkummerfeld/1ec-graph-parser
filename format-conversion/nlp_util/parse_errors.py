#!/usr/bin/env python

from collections import defaultdict

import pstree

class Parse_Error_Set:
	def __init__(self, gold=None, test=None, include_terminals=False):
		self.missing = []
		self.crossing = []
		self.extra = []
		self.POS = []
		self.spans = {}

		if gold is not None and test is not None:
			errors = get_errors(test, gold, include_terminals)
			for error in errors:
				if len(error) > 4:
					self.add_error(error[0], error[1], error[2], error[3], error[4])
				else:
					self.add_error(error[0], error[1], error[2], error[3])
	
	def add_error(self, etype, span, label, node, gold_label=None):
		error = (etype, span, label, node)
		if gold_label is not None:
			error = (etype, span, label, node, gold_label)
		if span not in self.spans:
			self.spans[span] = {}
		if label not in self.spans[span]:
			self.spans[span][label] = []
		self.spans[span][label].append(error)
		if etype == 'missing':
			self.missing.append(error)
		elif etype == 'crossing':
			self.crossing.append(error)
		elif etype == 'extra':
			self.extra.append(error)
		elif etype == 'diff POS':
			self.POS.append(error)

	def is_extra(self, node):
		if node.span in self.spans:
			if node.label in self.spans[node.span]:
				for error in self.spans[node.span][node.label]:
					if error[0] == 'extra':
						return True
		return False

	def __len__(self):
		return len(self.missing) + len(self.extra) + len(self.crossing) + (2*len(self.POS))

def get_errors(test, gold, include_terminals=False):
	ans = []

	gold_spans = []
	gold_POS = {}
	gold_span_set = defaultdict(lambda: 0)
	for span in gold:
		if span.is_terminal():
			if include_terminals:
				gold_POS[span.span] = span.label
			continue
		key = (span.span[0], span.span[1], span.label)
		gold_span_set[key] += 1
		gold_spans.append((key, span))

	test_spans = []
	test_span_set = defaultdict(lambda: 0)
	for span in test:
		if span.is_terminal():
			if include_terminals:
				tnode = span
				gold_label = gold_POS[tnode.span]
				if gold_label != tnode.label:
					ans.append(('diff POS', tnode.span, tnode.label, tnode, gold_label))
			continue
		key = (span.span[0], span.span[1], span.label)
		test_span_set[key] += 1
		test_spans.append((key, span))

	# Extra
	for key, span in test_spans:
		count = gold_span_set.get(key)
		if count is None or count == 0:
			ans.append(('extra', span.span, span.label, span))
		else:
			gold_span_set[key] -= 1

	# Missing and crossing
	for key, span in gold_spans:
		count = test_span_set.get(key)
		if count is None or count == 0:
			name = 'missing'
			for tkey, tspan in test_spans:
				if tkey[0] < key[0] < tkey[1] < key[1]:
					name = 'crossing'
					break
				elif key[0] < tkey[0] < key[1] < tkey[1]:
					name = 'crossing'
					break
			ans.append((name, span.span, span.label, span))
		else:
			test_span_set[key] -= 1
	return ans

def counts_for_prf(test, gold, include_root=False, include_terminals=False):
	# Note - currently assumes the roots match
	tcount = 0
	for node in test:
		if node.is_terminal() and not include_terminals:
			continue
		if node.parent is None and not include_root:
			continue
		tcount += 1
	gcount = 0
	for node in gold:
		if node.is_terminal() and not include_terminals:
			continue
		if node.parent is None and not include_root:
			continue
		gcount += 1
	match = tcount
	errors = Parse_Error_Set(gold, test, True)
	match = tcount - len(errors.extra)
	if include_terminals:
		match -= len(errors.POS)
	return match, gcount, tcount, len(errors.crossing), len(errors.POS)

if __name__ == '__main__':
	print "No unit testing implemented for Error_Set"
