/*
 * (c) Eric Mink 2018
 */
#include <iostream>
#include <cassert>
#include "treenode.cpp"
#include <vector>
#include <sstream>
#include <string>

// hardcoded input
static const std::string inputString = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"; // result should be 138

// ---------- HEADERS -----------
void testSimple();
std::vector<int> parse(std::string inputStr);

// ---------- PROGRAM -----------
int main(int argc, char* argv[]){
    testSimple();
    std::cout << "Hello World" << std::endl;

    std::vector<int> inputs = parse(inputString);
    struct VectorContainer vectorContainer;
    vectorContainer.vectorptr = new std::vector<int>;
    TreeNode rootNode = TreeBuilder::buildTree(vectorContainer);
    delete vectorContainer.vectorptr;
    std::cout << "metadata total in p and children: " << rootNode.getMetadataTotal() << std::endl;
}

std::vector<int> parse(std::string inputStr){
//  https://stackoverflow.com/questions/17724925/parse-string-containing-numbers-into-integer-array
    std::vector<int> inputs;
    std::stringstream ss(inputStr);
    int n; char ch;
    while(ss >> n){
        if(ss >> ch){
            inputs.push_back(n);
        } else {
            inputs.push_back(n);
        }
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
