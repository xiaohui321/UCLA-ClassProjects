// -----------------------------------------------------------------------------
// CS 137A HW 6 
// Xiaohui, Zhou
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Part I: Rule.prototype.makeCopyWithFreshVarNames() and
//         {Clause, Var}.prototype.rewrite(subst)
// -----------------------------------------------------------------------------

Rule.prototype.makeCopyWithFreshVarNames = function() {
	var newHead = this.head.makeCopyWithFreshVarNames();
	var newBody = this.body.map(function(arg){
		return arg.makeCopyWithFreshVarNames();
	});
	return new Rule(newHead,newBody);
};

Clause.prototype.makeCopyWithFreshVarNames = function() {
	var newArgs = this.args.map(function(arg){
		return arg.makeCopyWithFreshVarNames()
	});
	return new Clause(this.name,newArgs);
};

Var.prototype.makeCopyWithFreshVarNames = function() {
	return new Var(this.name + '~');
};


Clause.prototype.rewrite = function(subst) {
	var theArgs = this.args.map(function(arg){
		return arg.rewrite(subst)
	});
	return new Clause(this.name, theArgs);
};

Var.prototype.rewrite = function(subst) {
	return subst.lookup(this.name) || this;
};

// -----------------------------------------------------------------------------
// Part II: Subst.prototype.unify(term1, term2)
// -----------------------------------------------------------------------------

Subst.prototype.unify = function(term1, term2) {
	term1 = term1.rewrite(this);
	term2 = term2.rewrite(this);

	if(term1 instanceof Var || term2 instanceof Var) {
		if(term1 instanceof Var) 
			this.bind(term1,term2);
		else if(term2 instanceof Var) 
			this.bind(term2,term1);

	}else if (term1 instanceof Clause && term2 instanceof Clause) {
		if(term1.name != term2.name || term1.args.length != term2.args.length)
			throw new Error("unification failed");

		for(var i = 0; i < term1.args.length; i++)
			this.unify(term1.args[i],term2.args[i]);

	}else {
		throw new Error("unification failed");
	}

	for(var name in this.bindings) {
		this.bind(name,this.lookup(name).rewrite(this));
	}

	return this;
};
// -----------------------------------------------------------------------------
// Part III: Program.prototype.solve()
// -----------------------------------------------------------------------------
function Env(queries, rules, subst){
	this.queries = queries;
	this.rules = rules.map(function(rule){return rule.makeCopyWithFreshVarNames()});
	this.subst = subst.clone();
	this.next = [];
	this.prev = undefined;
}

Env.prototype.addNext = function(nextEnv){
	this.next.push(nextEnv);
	if(nextEnv) nextEnv.prev = this;
};

Program.prototype.solve = function() {
	var initialEnv = new Env(this.query, this.rules, new Subst());
	var currentEnv = initialEnv;
	var outputtedSolutions = [];
	var program = this;

	var getNextSolution = function(env) {
		if (!env) return false;
		if (currentEnv === env && currentEnv !== initialEnv)
			return getNextSolution(currentEnv.prev)

		currentEnv = env;
		if (env.queries.length > 0) {
			while (env.next.length < env.rules.length) {
				try {
					var nextQuery = env.queries[0];
					var nextRule = env.rules[env.next.length];
					var subst = env.subst.clone();
					subst.unify(nextQuery,nextRule.head);
					var queriesLeft = nextRule.body.concat(env.queries.slice(1)).map(
						function(query) { return query.rewrite(subst); }
					);
					var newEnv = new Env(queriesLeft, env.rules, subst);
					env.addNext(newEnv);
					return getNextSolution(newEnv);
				} catch(e) {
					env.addNext(undefined);
				}
			}
			return getNextSolution(env.prev);
		} else {
			var newSolution = env.subst.filter(program.getQueryVarNames());
			if (outputtedSolutions.indexOf(newSolution) < 0) {
				outputtedSolutions.push(newSolution);
				return env.subst;
			}
		}
	};

	return { next: function() {
		return getNextSolution(currentEnv);
	}};
};
