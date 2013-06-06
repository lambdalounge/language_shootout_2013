// Unnecessarily elaborate nucleotide counting in Roy
// by @rtfeldman
// Import underscore.js from Node.js
// since Roy doesn't have a standard library yet
var _ = require("underscore");
// Define the Base datatype and the Count type alias.
// (Malformed represents a bad input where the base could not be read.)
var Some = function(value_0) {
    if(!(this instanceof Some)) {
        return new Some(value_0);
    }
    this._0 = value_0;
};
var None = function() {
    if(!(this instanceof None)) {
        return new None();
    }
};
var A = function() {
    if(!(this instanceof A)) {
        return new A();
    }
};
var C = function() {
    if(!(this instanceof C)) {
        return new C();
    }
};
var G = function() {
    if(!(this instanceof G)) {
        return new G();
    }
};
var T = function() {
    if(!(this instanceof T)) {
        return new T();
    }
};
var emptyCount = {
    A: 0,
    C: 0,
    G: 0,
    T: 0
};
// Convert a Base to a Count
var toCount = function(maybeBase) {
    return (function(_m) {
        if(_m instanceof None) {
            return emptyCount;
        } else if(_m instanceof Some) {
            var base = _m._0;
            return (function(_m) {
        if(_m instanceof A) {
            return {
        A: 1,
        C: 0,
        G: 0,
        T: 0
    };
        } else if(_m instanceof C) {
            return {
        A: 0,
        C: 1,
        G: 0,
        T: 0
    };
        } else if(_m instanceof G) {
            return {
        A: 0,
        C: 0,
        G: 1,
        T: 0
    };
        } else if(_m instanceof T) {
            return {
        A: 0,
        C: 0,
        G: 0,
        T: 1
    };
        }
    })(base);
        }
    })(maybeBase);
};
// Add the contents of two Counts to produce a new Count
var addCounts = function(one, two) {
    return {
        A: one.A + two.A,
        C: one.C + two.C,
        G: one.G + two.G,
        T: one.T + two.T
    };
};
// Convert a String character to a Base
// (using the most elegant approach ever beheld)
var toBase = function(char) {
    return (function() {
        if(char == "A") {
            return Some(A());
        } else {
            return (function() {
                if(char == "C") {
                    return Some(C());
                } else {
                    return (function() {
                        if(char == "G") {
                            return Some(G());
                        } else {
                            return (function() {
                                if(char == "T") {
                                    return Some(T());
                                } else {
                                    return None();
                                }
                            })();
                        }
                    })();
                }
            })();
        }
    })();
};
// Use higher-order functions for the rest
var charToCount = _.compose(toCount, toBase);
var sumCounts = function(counts) {
    return _.reduce(counts, addCounts, emptyCount);
};
var strToCounts = function(str) {
    return sumCounts(_.map(str, charToCount));
};
// Actually run the count
var inputStr = process.argv[2];
console.log(strToCounts(inputStr));
//@ sourceMappingURL=dna.js.map
