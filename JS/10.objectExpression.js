"use strict";

var VARIABLES = {
        'x': 0,
        'y': 1,
        'z': 2,
};

function Const(x) {
        this.getValue = function() {
                return x;
        }
}

Const.prototype.toString = function() {
        return this.getValue().toString();
}

Const.prototype.evaluate = function() {
        return this.getValue();
}

function Variable(name) {
        var num = VARIABLES[name];
        this.getName = function() {
                return name;
        }
        this.getNum = function() {
                return num;
        }
}

Variable.prototype.toString = function() {
        return this.getName();
}

Variable.prototype.evaluate = function() {
        return arguments[this.getNum()];
}

function Operation() {
        var operands = Array.prototype.slice.call(arguments);
        this.getOperands = function() {
                return operands;
        }
}

Operation.prototype.toString = function() {
        return this.getOperands().join(" ") + " " + this.getSymbol();
}

Operation.prototype.evaluate = function() {
        var operationArgs = arguments;
        var result = this.getOperands().map(function(value) {
                return value.evaluate.apply(value, operationArgs)
        });
        return this.count.apply(this, result);
}

function NewOperation(count, symbol) {
        this.count = count;
        this.getSymbol = function() {
                return symbol;
        }
}

NewOperation.prototype = Operation.prototype;

function makeNewOperation(count, symbol) {
        var result = function() {
                Operation.apply(this, arguments);
        }
        result.prototype = new NewOperation(count, symbol);
        return result;
}

var Add = makeNewOperation(
        function(a, b) {
                return a + b;
        },
        '+'
);

var Subtract = makeNewOperation(
        function(a, b) {
                return a - b;
        },
        '-'
);

var Multiply = makeNewOperation(
        function(a, b) {
                return a * b;
        },
        '*'
);

var Divide = makeNewOperation(
        function(a, b) {
                return a / b;
        },
        '/'
);

var Negate = makeNewOperation(
        function(a) {
                return -a;
        },
        "negate"
);

var Square = makeNewOperation(
        function(a) {
                return a * a;
        },
        "square"
);

var Sqrt = makeNewOperation(
        function(a) {
                return Math.sqrt(Math.abs(a));
        },
        "sqrt"
);
