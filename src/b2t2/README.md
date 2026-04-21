# B2T2 Implementation in Hazel

This directory contains Hazel's implementation of the B2T2 (Brown Benchmark for Table Types) benchmark.

## What is B2T2?

B2T2 is a language design benchmark for evaluating type systems for table programming. It provides a standardized framework to compare the expressive power and diagnostic quality of different programming languages and systems when handling tabular data operations.

The benchmark was created by researchers at Brown University and is documented in the paper:

**"Types for Tables: A Language Design Benchmark"**  
Authors: Kuang-Chen Lu, Ben Greenman, Shriram Krishnamurthi  
Published in: The Art, Science, and Engineering of Programming, 2022

- **Paper**: https://cs.brown.edu/~sk/Publications/Papers/Published/lgk-b2t2/
- **Repository**: https://github.com/brownplt/B2T2

## What is this Directory?

This directory contains Hazel's implementation and evaluation of the B2T2 This implementation demonstrates how well Hazel's type system handles table programming constructs.

The implementation includes:
- **Datasheet** (`Datasheet.md`): A comprehensive evaluation of how Hazel addresses each component of the B2T2 benchmark
- **Implementation** (`Datasheet.re`): Code used to turn the markdown datasheet into a documentation slide in the editor
- **Documentation Slides** (`slides/`): Interactive examples demonstrating B2T2 concepts in Hazel
- **Slides Module** (`Slides.re`): Aggregates all B2T2 slides for integration into Hazel's documentation system

## B2T2 Benchmark Components

The B2T2 benchmark consists of several key components that implementations must address:

1. **Table Definition**: Specification of what constitutes a table in the language
2. **Example Tables**: Various table structures that must be expressible
3. **Table API**: A standard library of table operations (filtering, joining, grouping, etc.)
4. **Example Programs**: Real-world programs that manipulate tables
5. **Error Scenarios**: Common programming errors and how well the type system catches them
6. **Datasheet**: Structured evaluation of the implementation's capabilities

## Documentation Slides

The slides are organized in `Slides.re` and automatically loaded into Hazel's documentation system via `src/web/init/Init.re`.
