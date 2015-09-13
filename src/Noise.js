
/* global exports, Noise */
"use strict;"

// module Noise

exports.seed = function(val){
    return function(){
            return processing.noiseSeed(val);
    };
};

exports.noise1d = function(x){
    return function(){
            return processing.noise(x);
    };
};



exports.noise2d = function(x){
    return function(y){
        return function(){
            return processing.noise(x,y);
        }
    };
};


exports.noise3d = function(x){
    return function(y){
        return function(z){
            return function(){
                return processing.noise(x,y,z);
            };
        };
    };
};


