
/* global exports, Noise */
"use strict;"

// module Noise


exports.simplex2 = function(x){
    return function(y){
        return function(){
            return noise.simplex2(x,y);
        }
    };
};


exports.simplex3 = function(x){
    return function(y){
        return function(z){
            return function(){
                return noise.simplex3(x,y,z);
            };
        };
    };
};


exports.perlin2 = function(x){
    return function(y){
        return function(){
            return noise.perlin2(x,y);
        }
    };
};


exports.perlin3 = function(x){
    return function(y){
        return function(z){
            return function(){
                return noise.perlin3(x,y,z);
            };
        };
    };
};

exports.seed = function(val){
    return function(){
        return noise.seed(val);
    };
};
