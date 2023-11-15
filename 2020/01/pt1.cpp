#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>

/*
 * Inputs are the calories each elf is carrying.
 * Blank lines separate elves' backpacks.
 *
 * Goal: find the elf carrying the most calories.
 *       How many calories is that elf carrying?
 */

class Backpack {
    private:
        int index = -1;
        std::vector<int> calories;
        int total = -1;

    public: 
        Backpack ( int index, std::vector<int> calories ) {
            this->index = index;
            this->calories = calories;
        }

        int sum_calories () {
            if (this->total > 0){
                // already computed.
                return this->total;
            }

            int total = 0;
            std::for_each(this->calories.begin(),
                                 this->calories.end(),
                                 [&total](int n){
                                    total += n;
                                 });
            return total;
        }
};

int solve_pt1(std::string input_file){
    /*
     * I know i am solving this overcomplicatedly and
     * suboptimally in memory consumption. But I want to 
     * freshen up my cpp object oriented skills.
     */
    std::ifstream input(input_file);
    std::vector<Backpack> all_elves;
    std::string line; int index = 0; 
    std::vector<int> current_calories;
    while (getline(input, line)){
        if (line.empty()){
            // end current elf.
            all_elves.emplace_back(index, current_calories);

            // next elf.
            // reusing the vector should be okay because the Backpack constructor
            // copies the vector elements, when it is called by emplace_back.
            current_calories.clear();
            index += 1;
        } else {
            int cal; std::istringstream(line) >> cal;
            current_calories.push_back(cal);
        }
    }

    Backpack max_elf = *std::max_element(all_elves.begin(), all_elves.end(), 
             [](Backpack& a, Backpack& b) { return (a.sum_calories() < b.sum_calories()); }
            );
    
    return max_elf.sum_calories();
}

int main( int argc, char** argv ){
    Backpack b (1, {1, 2, 3});
    std::cout << "hello world" << std::endl;
    std::cout << "Backpack b is carrying "
         << b.sum_calories() << " calories"
         << std::endl;

    if (argc < 2){
        std::cout << "Usage: EXECUTABLE 'pt1' 'input.txt'" << std::endl;
        exit(0);
    }

    if (std::string(argv[1]) == "pt1"){
        int solution1 = solve_pt1(std::string(argv[2]));
        std::cout << "Solution Pt1: " << solution1 << std::endl;
    }
}
