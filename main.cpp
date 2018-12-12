/*
 * (c) Eric Mink 2018
 */
#include <iostream>
#include "treenode.cpp"

int main(int argc, char* argv[]){
    std::cout << "Hello World" << std::endl;
    TreeNode t = TreeNode(3);
    t.addMetadata(3);
    t.addMetadata(4);
    std::cout << "metadata sum in t: " << t.getMetadataSum() << std::endl;
}
