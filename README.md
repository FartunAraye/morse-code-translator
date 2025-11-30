# morse-code-translator
A Haskell program that encodes text to Morse code and decodes Morse code back to text using both lookup tables and binary tree structures.

Features
Text to Morse encoding - Converts words and sentences into Morse code with proper gap spacing
Morse to text decoding - Translates Morse code back to readable text using lookup tables
Binary tree decoder - Alternative decoding method using a tree structure for efficient lookups
Tree generation - Builds binary trees from Morse code tables (left branch = dit, right branch = dah)
Table generation - Extracts lookup tables from tree structures by traversing paths
Bracket parser - Bonus feature that parses and validates well-bracketed strings

How it works
Encoding: Takes text and converts each character to its Morse code equivalent, separating characters with short gaps and words with medium gaps.
Decoding with tables: Splits Morse code by gaps and looks up each code in a table to find the corresponding character.
Decoding with trees: Traverses a binary tree where dits go left and dahs go right until reaching a leaf node containing the character.
