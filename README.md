[![CI](https://github.com/certainty/cl-braces/actions/workflows/ci.yml/badge.svg)](https://github.com/certainty/cl-braces/actions/workflows/ci.yml)

# CL-Braces a compiler and virtual machine for a small go-like language.

## State

While I’m making good progress most of this is in very very early development and many of the features only exist in my head. I totally expect this to take maybe a year till this is somewhere in a state I can show it, at the current rate of development. Well I guess that’s just it. If you still feel interested or even want to contribute, please get in contact. I’m happy to nerd out about this :)

## Goals

Implement a bytecode compiler and virtual machine for a minimal go-like language. Both the compiler and the VM aim to offer sophisticated introspection capabilities, which allow everyone interested in programming language implementations to peek into every state of the compilation & exection process.

In total this aims to be a learning environment for PL implementations (also for myself)

## Non-Goals

A programming language that is used for real world applications, other than the ones build within this project.

## Decisions

Use a golike language to show implementations of compilers for the typical langauge constructs, while still being fairly minimal.
Don’t only implement toy examples but try to give real implementions, that resemble what you would do in a compiler aimed for a real language
