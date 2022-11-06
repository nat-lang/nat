[id] = \x . x;

[V] = \y:<e> . \x:<e> . \e:<v> . V(e)(y)(x);
[NP] = NP:<e>;
[PF] = \t:<i> . \p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e);

[bindt] = \t:<i>;
[t] = t;

C:<i,t>;
[PAST] = \c:<i,t> . \t:<i> . \i:<i,t> . exists t' . (t' < t) & (c t') & (i t');

[bindt'] = \t':<i>;
[t'] = t';

March44BC: <i>;
[ontheidesofMarch44BC] = \t:<i> . t subs March44BC;

[always] = \q:<i,t> . \p:<i,t> . forall t':<i> . (q t') => (p t')
