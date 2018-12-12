#include <iostream>
#include <vector>
#include <numeric>
#include <cassert>

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

struct VectorContainer {
    std::vector<int>* vectorptr;
}

class TreeBuilder {
    public:
        // modifies inputs content
        static TreeNode buildTree(VectorContainer & inputs){
            int i = 0;
            int numChildren = inputs.vector.get(i++);
            int numMetadata = inputs.vector.get(i++);
            TreeNode root = TreeNode(numChildren);

            // set up children count by passing the tail (consuming the inputs)
            std::vector<int>* tailptr = new std::vector<int>;
            *tailptr = std::vector<int>(inputs->begin()+2, inputs->end());
            for (int j=0; j<numChildren; j++){
                TreeNode child = buildTree(&tail); // tail gets modified
                root.addChild(child);
            }

            // modify inputs
            delete inputs.vectorptr;
            inputs.vectorptr = tailptr;
            // inputs.vectorptr should now contain the remaining string that has not yet been parsed
            assert(root.metadata.size() == numMetadata);
            return root;
        }
};
