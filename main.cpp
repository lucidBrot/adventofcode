/*
 * (c) Eric Mink 2018
 */
#define MY_MAIN true
#include <iostream>
#include <cassert>
#include <vector>
#include <sstream>
#include <string>
#include "main.h"
#include "treenode.cpp"

// hardcoded input
static const std::string inputString = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"; // result should be 138

// ---------- PROGRAM -----------
int main(int argc, char* argv[]){
    testSimple(); std::cout << std::endl;
    testGiven(); std::cout << std::endl;

    // actually run
    std::vector<int>* inputs = new std::vector<int>(parse(inputString));
    std::cout << "Finished Parsing:" << std::endl << "\t";
    printVector(*inputs);
    std::cout << std::endl;
    struct VectorContainer vectorContainer;
    std::cout << "Declared empty struct at " << &vectorContainer << std::endl;
    vectorContainer.vectorptr = inputs;
    TreeNode rootNode = TreeBuilder::buildTree(vectorContainer);
    std::cout << "metadata total in p and children: " << rootNode.getMetadataTotal() << std::endl;
}

std::vector<int> parse(std::string inputStr){
//  https://stackoverflow.com/questions/17724925/parse-string-containing-numbers-into-integer-array
    std::vector<int> inputs;
    std::stringstream stream(inputStr);
    int n;
    while (stream >> n){
        inputs.push_back(n);
    }
    return inputs;
}

void testSimple(){
    std::cout << "--- Test: testSimple() start ---" << std::endl << std::endl;
    TreeNode t = TreeNode(0);
    t.addMetadata(3);
    t.addMetadata(4);

    TreeNode p = TreeNode(1);
    p.addMetadata(1);
    p.addChild(t);

    int total = p.getMetadataTotal();
    std::cout << "metadata total in p and children: " << total << std::endl;
    assert(total == 8);
    std::cout << "--- Test: testSimple() done ---" << std::endl;
}

void testGiven(){
    std::cout << "--- Test: testGiven() start ---" << std::endl << std::endl;
    const std::string inputString = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"; // result should be 138
    std::vector<int>* parsedptr = new std::vector<int>(parse(inputString));
    struct VectorContainer vectorContainer = (struct VectorContainer){.vectorptr = parsedptr};
    TreeNode rootNode = TreeBuilder::buildTree(vectorContainer);
    int total = rootNode.getMetadataTotal();
    std::cout << "total: " << total << std::endl;
    assert(total == 138);
    std::cout << "--- Test: testGiven() done ---" << std::endl;
}

void printVector(std::vector<int> v){
    for (std::vector<int>::const_iterator i = v.begin(); i != v.end(); i++){
        std::cout << *i << ' ';
    }
}
