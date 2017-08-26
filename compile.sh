#!/bin/bash

#SBCL instruction
#Other lisps might require other invocation

sbcl --eval "(asdf:make :clox)"
