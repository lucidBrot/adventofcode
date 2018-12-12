#include <iostream>
#include <vector>

class TreeNode {

    private:
        int numChildren;
        std::vector<TreeNode> children;
        std::vector<int> metadata;

    public:
    
    TreeNode(int numChildren){
        this->numChildren = numChildren;
        std::cout << numChildren << "-child node created" << std::endl;
    }

};
