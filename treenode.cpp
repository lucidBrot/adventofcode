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
};

class TreeBuilder {
    public:
        // modifies inputs content
        static TreeNode buildTree(VectorContainer & inputs){
            std::cout << "[B]: vectorptr = " << inputs.vectorptr << std::endl;
            int i = 0;
            int numChildren = inputs.vectorptr->at(i++);
            int numMetadata = inputs.vectorptr->at(i++);
            std::cout << "[Checkpoint Recursion]" << std::endl;
            TreeNode root = TreeNode(numChildren);

            std::cout << "creating TreeNode with numChildren=" << numChildren
                << "and numMetadata=" << numMetadata << std::endl;

            // consume first two entries which are numChildren and numMetadata
            std::vector<int>* tailptr = new std::vector<int>;
            *tailptr = std::vector<int>(inputs.vectorptr->begin()+2, inputs.vectorptr->end());

            // pack tail into vectorContainer so the next buildTree call operates on what remains for the children
            delete inputs.vectorptr;
            inputs.vectorptr = tailptr;

            // set up children count by passing the tail (consuming the inputs)
            for (int j=0; j<numChildren; j++){
                TreeNode child = buildTree(inputs); // tail gets modified so what remains is for the next child
                root.addChild(child);
            }

            // at this point, inputs.vectorptr contains only the numbers for the this.parent.nextchild and so on. In other words:
            // inputs.vectorptr should now contain the remaining string that has not yet been parsed
            // recursion stops when a node has no children. Then the next numMetadata numbers are the metadata
            for(int j=0; j<numMetadata; j++){
                root.addMetadata(inputs.vectorptr->back());
                inputs.vectorptr->pop_back();
            }
            return root;
        }
};
