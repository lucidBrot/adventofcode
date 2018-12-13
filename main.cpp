#include <iostream>
#include <Eigen/Dense>
using Eigen::MatrixXd;
#include <sstream>
#include <string>

enum class Cart {Up=0, Down=1, Left=2, Right=3, Crashed=4};

void parseCartsPositions(std::string filecontents){
    std::istringstream f(filecontents);
    std::string line;
    unsigned int x = 0;
    unsigned int y = 0;
    while (std::getline(f, line)) {
        for (char& c : line){
            // read cart positions and nothing else
            Cart cart;
            switch(c){
                case 'v':
                    // down cart
                    cart = Cart::Down;
                    std::cout << "c is of enum : " << static_cast<int>(cart) << std::endl;
                    break;
                default:
                    // not a cart, but increases coordinate nonetheless
                    break;
            }
            x++;
        }
        x=0;
        y++;
    }
}

int main() { 
  MatrixXd m(2,2); 
  m(0,0) = 3; 
  m(1,0) = 2.5; 
  m(0,1) = -1; 
  m(1,1) = m(1,0) + m(0,1); 
  std::cout << m << std::endl; 

  parseCartsPositions("asdv\nfgv");
}
