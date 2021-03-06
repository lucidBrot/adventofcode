#include <iostream>
#include <vector>
#include <numeric>
#include <cassert>

#include "main.h" // for printing vector

#ifndef MY_MAIN
#include "main.cpp"
#endif

class TreeNode {

    private:
        int numChildren;
        std::vector<TreeNode> children;
        std::vector<int> metadata;

    public:
        TreeNode(int numChildren){
            this->numChildren = numChildren;
#if verbose
            std::cout << "[TreeNode Constructor]: " << numChildren << "-child TreeNode created" << std::endl;
#endif
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

        int getNodeValue(){
            if (this->numChildren == 0){
                return this->getMetadataSum();
            } else {
                int value = 0;
                for(size_t m : this->metadata){
                    if (m >= this->children.size() || m==0){
                        continue;   
                    }
                    TreeNode child = this->children.at(m-1);
                    value += child.getNodeValue();
                }
            }
        }

};

struct VectorContainer {
    std::vector<int>* vectorptr;
};

class TreeBuilder {
    public:
        // modifies inputs content
        static TreeNode buildTree(VectorContainer & inputs){
#if verbose
            std::cout << std::endl << "------------------------------------" << std::endl;
            std::cout << "input tail: "; printVector(*inputs.vectorptr); std::cout << std::endl;
            std::cout << "------------------------------------" << std::endl;
#endif

            int numChildren = inputs.vectorptr->at(0);
            int numMetadata = inputs.vectorptr->at(1);
            ///std::cout << "[Checkpoint Recursion]" << std::endl;
            TreeNode root = TreeNode(numChildren);

            //std::cout << "creating TreeNode with numChildren=" << numChildren << " and numMetadata=" << numMetadata << std::endl;

            // consume first two entries which are numChildren and numMetadata
            std::vector<int>* tailptr = new std::vector<int>();
            *tailptr = std::vector<int>(inputs.vectorptr->begin()+2, inputs.vectorptr->end());

#if verbose
            std::cout << "(" << numChildren << ", " << numMetadata << ")  debug output 1" << std::endl;
#endif
            // pack tail into vectorContainer so the next buildTree call operates on what remains for the children
            delete inputs.vectorptr;
            inputs.vectorptr = tailptr;
#if verbose
            std::cout <<  "(" << numChildren << ", " << numMetadata << ")  debug output 2" << std::endl;
#endif

            // set up children count by passing the tail (consuming the inputs)
            for (int j=0; j<numChildren; j++){
                TreeNode child = buildTree(inputs); // tail gets modified so what remains is for the next child
                root.addChild(child);
            }
#if verbose
            std::cout << "(" << numChildren << ", " << numMetadata << ")  debug output 3" << std::endl;
#endif

            // at this point, inputs.vectorptr contains only the numbers for the this.parent.nextchild and so on. In other words:
            // inputs.vectorptr should now contain the remaining string that has not yet been parsed
            // recursion stops when a node has no children. Then the next numMetadata numbers are the metadata
#if verbose
            std::cout <<  "(" << numChildren << ", " << numMetadata << ")  ";
            std::cout << "Tail before metadata consumption: "; printVector(*inputs.vectorptr); std::cout << std::endl; 
#endif

            int consumeTailBy = 0;
            for(int j=0; j<numMetadata; j++){
                root.addMetadata(inputs.vectorptr->at(0));
                inputs.vectorptr->erase(inputs.vectorptr->begin());
                consumeTailBy++;
            }
#if verbose
            std::cout <<  "(" << numChildren << ", " << numMetadata << ")  ";
            std::cout << "Tail after metadata consumption: "; printVector(*inputs.vectorptr); std::cout << std::endl; 
#endif
            return root;
        }
};
