#include <iostream>
#include <vector>
#include <numeric>

class TreeNode {

    private:
        int numChildren;
        std::vector<TreeNode> children;
        std::vector<int> metadata;

    public:
        TreeNode(int numChildren){
            this->numChildren = numChildren;
            std::cout << "[TreeNode Constructor]: " << numChildren << "-child TreeNode created" << std::endl;
        }

        void addChild(TreeNode child){
            this->children.push_back(child);
        }

        void addMetadata(int datum){
            this->metadata.push_back(datum);
        }

        int getMetadataSum(){
            return std::accumulate(this->metadata.begin(), this->metadata.end(), 0);
        }

        /*
         * get metadata sum of this node and its children
         */
        int getMetadataTotal(){
            int metadataOfChildren = 0;
            for (TreeNode child : this->children){
                metadataOfChildren += child.getMetadataTotal();
            }
            return metadataOfChildren + this->getMetadataSum();
        }

};

class TreeBuilder {
    public:
        static TreeNode buildTree(std::vector<int> inputs){
            TreeNode root = TreeNode(1);
            return root;
        }
};
