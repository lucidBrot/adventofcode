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
            std::cout << numChildren << "-child TreeNode created" << std::endl;
        }

        void addChild(TreeNode child){
            this->children.push_back(child);
        }

        void addMetadata(int datum){
            this->metadata.push_back(datum);
        }

};
