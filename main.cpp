#include <iostream>
#include <Eigen/Dense>
#include <Eigen/Sparse>
using Eigen::MatrixXd;
typedef Eigen::SparseMatrix<int, Eigen::RowMajor> SMatrix;
#include <sstream>
#include <string>
#include <cassert>
#include <tuple>
#include <fstream>
#include <streambuf>
#include <exception>
#include <cstdlib>
#include <map>
#include <iterator>
#include <Eigen/Core>

struct CartCrashedException : public std::exception
{
    public:
        long unsigned int m_x, m_y;

	const char * what () const throw ()
    {
    	return "Cart Crashed!";
    }

    CartCrashedException(long unsigned int x, long unsigned int y){
        m_x = x;
        m_y = y;
    }
};

enum class Cart : int {None=0, Up, Down, Left, Right, Crashed};
const Cart InverseCart[] = {Cart::None, Cart::Up, Cart::Down, Cart::Left, Cart::Right, Cart::Crashed};

SMatrix parseCartsPositions(std::string filecontents, long unsigned int maxX, long unsigned int maxY){
    SMatrix cartStorage; cartStorage.resize(maxY, maxX); cartStorage.reserve(20);
    std::istringstream f(filecontents);
    std::string line;
    long unsigned int x = 0;
    long unsigned int y = 0;
    while (std::getline(f, line)) {
        for (char& c : line){
            // read cart positions and nothing else
            Cart cart;
            switch(c){
                case 'v':
                    // down cart
                    cart = Cart::Down;
                    break;
                case '^':
                    cart = Cart::Up;
                    break;
                case '<':
                    cart = Cart::Left;
                    break;
                case '>':
                    cart = Cart::Right;
                    break;

                default:
                    // not a cart, but increases coordinate nonetheless
                    cart = Cart::None;
                    break;
            }
            if (cart != Cart::None){
                cartStorage.coeffRef(y,x) = static_cast<int>(cart);
            }
            x++;
        }
        x=0;
        y++;
    }
    assert( maxY == y);
    return cartStorage;

}

enum class Track : int {None=0, Slash, Backslash, Minus, Plus, Pipe};
const Track InverseTrack[] = {Track::None, Track::Slash, Track::Backslash, Track::Minus, Track::Plus, Track::Pipe};

SMatrix parseTracks(std::string filecontents, unsigned int maxX, unsigned int maxY){

    SMatrix trackStorage; trackStorage.resize(maxY, maxX); trackStorage.reserve(20);
    std::istringstream f(filecontents);
    std::string line;
    unsigned int x = 0;
    unsigned int y = 0;
    while (std::getline(f, line)) {
        for (char& c : line){
            // read track positions and nothing else
            Track track;
            switch(c){
                case '/':
                    track = Track::Slash; break;
                case '\\':
                    track = Track::Backslash; break;
                case '-':
                    track = Track::Minus; break;
                case '+':
                    track = Track::Plus; break;
                case '|':
                    track = Track::Pipe; break;
                default:
                    track = Track::None; break;
            }

            if(track != Track::None){
                trackStorage.coeffRef(y,x) = static_cast<int>(track);
            }
            x++;
        }
        x=0;
        y++;
    }
    assert( maxY == y);
    return trackStorage;
}

std::string readFileToString(std::string filename){
    std::ifstream t(filename);
    std::stringstream buffer;
    buffer << t.rdbuf();
    return buffer.str();
}

struct TrackWithPos {
    Track track;
    long unsigned int x;
    long unsigned int y;
};

struct CartWithPos {
    Cart cart;
    long unsigned int x;
    long unsigned int y;
};

struct TrackWithPos getNextTrackForCart(SMatrix trackData, size_t cartX, size_t cartY, Cart cartOrientation){
    size_t x=0, y=0;
    switch(cartOrientation){
        case Cart::Up:
            y=-1;
            break;
        case Cart::Down:
            y=1;
            break;
        case Cart::Left:
            x=-1;
            break;
        case Cart::Right:
            x=1;
            break;
        case Cart::Crashed:
            assert(0 && "A crashed cart has no next destination!");
            break;
        case Cart::None:
            assert(0 && "The cart isn't real");
            break;
    }

    size_t nextX = cartX+x;
    size_t nextY = cartY+y;

    int trackNum = trackData.coeffRef(nextY, nextX);
    struct TrackWithPos twp = {InverseTrack[trackNum], // track
                                nextX, // x
                                nextY // y
                                };
    return twp;

}

enum class Decision : int {left=0, straight, right};

// function :: cart coordinates, orientation, fact that next is an intersection --> which orientation is next
Cart getOrientationAfterIntersection(Cart originalOrientation, Decision latestCartDecision){
    assert(originalOrientation != Cart::None && originalOrientation != Cart::Crashed);
    
    Decision decision = latestCartDecision;

    if(decision==Decision::straight){
        return originalOrientation;
    }

    // compute assuming decision==left
    Cart leftDecision;
    // We know that cart drives in direction of its orientation. So there lies the intersection.
    switch(originalOrientation){
        case Cart::Up:
            leftDecision = Cart::Left;
            break;
        case Cart::Down:
            leftDecision = Cart::Right;
            break;
        case Cart::Left:
            leftDecision = Cart::Down;
            break;
        case Cart::Right:
            leftDecision = Cart::Up;
            break;
        default: // TODO: do default case, and make sure to invert leftdecision i decision == right
            assert(0 && "Broken Orientation parameter!");
            break;
    }

    if(decision==Decision::left){
        return leftDecision;
    }

    auto invertDecision = [](Cart decision){
        switch(decision){
            case Cart::Up:
                return Cart::Down;
            case Cart::Down:
                return Cart::Up;
            case Cart::Left:
                return Cart::Right;
            case Cart::Right:
                return Cart::Left;
            default:
                assert(0);
        }
    };

    //: if(decision==Decision::right){
    return invertDecision(leftDecision);
}


static int yee = 0;
struct CartWithPos getNextCart(struct CartWithPos originalCart, struct TrackWithPos nextTrack, Decision previousCartDecision){
    assert(originalCart.cart!=Cart::Crashed); assert(originalCart.cart!=Cart::None);
    assert(nextTrack.track != Track::None);
    struct CartWithPos nextCart;
    nextCart.x = nextTrack.x;
    nextCart.y = nextTrack.y;

    //std::cout << "[getNextCart] " << yee++ << std::endl;

    // handle intersections
    if(nextTrack.track == Track::Plus){
        nextCart.cart = getOrientationAfterIntersection(originalCart.cart, previousCartDecision);
        return nextCart;
    }

    switch(originalCart.cart){
        case Cart::Up:
            {
                switch(nextTrack.track){
                    case Track::Slash:
                        nextCart.cart = Cart::Right;
                        break;
                    case Track::Backslash:
                        nextCart.cart = Cart::Left;
                        break;
                    case Track::Minus:
                        std::cout << std::flush;
                        assert(0 && "This should not happen: Moving upwards onto horizontal track!");
                        break;
                    case Track::Pipe:
                        nextCart.cart = Cart::Up;
                        break;
                    case Track::None:
                        assert(0);
                        break;
                    case Track::Plus:
                        assert(0);
                        break;

                }
            }
            break;
        case Cart::Down:
            {
                switch(nextTrack.track){
                    case Track::Slash:
                        nextCart.cart = Cart::Left;
                        break;
                    case Track::Backslash:
                        nextCart.cart = Cart::Right;
                        break;
                    case Track::Minus:
                        std::cout << std::flush;
                        assert(0 && "This should not happen: Moving down onto horizontal track!");
                        break;
                    case Track::Pipe:
                        nextCart.cart = Cart::Down;
                        break;
                    case Track::None:
                        assert(0);
                        break;
                    case Track::Plus:
                        assert(0);
                        break;

                }
            }
            break;
        case Cart::Right:
            {
                switch(nextTrack.track){
                    case Track::Slash:
                        nextCart.cart = Cart::Up;
                        break;
                    case Track::Backslash:
                        nextCart.cart = Cart::Down;
                        break;
                    case Track::Minus:
                        nextCart.cart = Cart::Right;
                        break;
                    case Track::Pipe:
                        assert(0 && "Bad things are happening: Moving horizontally onto a pipe!");
                        break;
                    case Track::None:
                        assert(0);
                        break;
                    case Track::Plus:
                        assert(0);
                        break;

                }
            }
            break;
        case Cart::Left:
            {
                switch(nextTrack.track){
                    case Track::Slash:
                        nextCart.cart = Cart::Down;
                        break;
                    case Track::Backslash:
                        nextCart.cart = Cart::Up;
                        break;
                    case Track::Minus:
                        nextCart.cart = Cart::Left;
                        break;
                    case Track::Pipe:
                        assert(0 && "Bad things are happening: Moving horizontallii onto a pipe!");
                        break;
                    case Track::None:
                        assert(0);
                        break;
                    case Track::Plus:
                        assert(0);
                        break;

                }
            }
            break;
        // these won't happen. Let the compiler know that it does not need warnings.
        case Cart::Crashed:
            [[fallthrough]];
        case Cart::None:
            assert(0);
    }

    return nextCart;
}

Decision incDecision(Decision decision){
    if (decision == Decision::right){return Decision::left;}
    else{return (Decision)(((int)decision)+1);}
}

// modifies carts SMatrix
void removeCrashed(SMatrix & carts){
    for (int k=0; k<carts.outerSize(); ++k){
        for (SMatrix::InnerIterator cart(carts,k); cart; ++cart){
            if(cart.value()==static_cast<int>(Cart::Crashed)){
                carts.coeffRef(cart.row(), cart.col()) = static_cast<int>(Cart::None);
            }
        }
    }
}

bool isLessThanTwoCartsLeft(SMatrix carts){
    int numCarts = 0;
    for (int k=0; k<carts.outerSize(); ++k){
        for (SMatrix::InnerIterator cart(carts,k); cart; ++cart){
            if(cart.value()!=static_cast<int>(Cart::Crashed) && cart.value()!=static_cast<int>(Cart::None)){
                numCarts++;
                if(numCarts > 1){return false;}
            }
        }
    }
    return true;
}

std::string anyCartsCoordsAsString(SMatrix carts){
    for (int k=0; k<carts.outerSize(); ++k){
        for (SMatrix::InnerIterator cart(carts,k); cart; ++cart){
            if(cart.value()==static_cast<int>(Cart::Crashed) 
                    || cart.value()==static_cast<int>(Cart::None)){
                continue;
            }
            std::stringstream ret;
            ret << "(";
            ret << cart.col() << ", " << cart.row() << ")";
            return ret.str();
        }
    }
    return "No Coors";
}
void moveCarts(SMatrix & carts, SMatrix & trackData, SMatrix & cartsDecisions, int version){
    // create copies so that new cart positions are not considered in previous runs.
    SMatrix originalCarts = SMatrix(carts);

    // get nonzero entries - since zero stands for None
    std::cout << "Carts: " << std::endl;
    for (int k=0; k<originalCarts.outerSize(); ++k)
        for (SMatrix::InnerIterator it(originalCarts,k); it; ++it)
        {

            if (version == 2 && false){ // true to stop instantly, false to stop at end of tick
                // check if only one cart is left
                if(isLessThanTwoCartsLeft(carts)){

                    std::cout << std::endl <<  "Fin: " << anyCartsCoordsAsString(carts) << std::endl;
                    return;
                }
            }

            it.row();   // row index
            it.col();   // col index (here it is equal to k)
            it.index(); // inner index, here it is equal to it.row()

            // not sure if this is neccessary
            if(it.value() == static_cast<int>(Cart::None)){
                continue;
            }
            std::cout << "(" << it.col() << ", " << it.row() << ")[" << it.value() << "]\t";

            // find the next track to be driven on
            Cart cart = InverseCart[it.value()];
            struct TrackWithPos nextTrack = getNextTrackForCart(trackData, it.col(), it.row(), cart);
            struct CartWithPos originalCart = {
                cart, // cart
                (long unsigned int) it.col(), // x
                (long unsigned int) it.row(), // y
            };

            // figure out what decision the cart has taken last time
            ///std::cout << std::endl << "Decision Matrix: " << cartsDecisions << std::endl << std::endl;
            Decision previousCartDecision = (Decision) cartsDecisions.coeffRef(originalCart.y, originalCart.x);

            // figure out where to go next
            struct CartWithPos nextCart = getNextCart(originalCart, nextTrack, previousCartDecision);
            bool crashed = false;
            // check if crash
            if (carts.coeffRef(nextCart.y, nextCart.x) != static_cast<int>(Cart::None)){
                // crash happened
                carts.coeffRef(originalCart.y, originalCart.x) = static_cast<int>(Cart::None);
                carts.coeffRef(nextCart.y, nextCart.x) = static_cast<int>(Cart::Crashed);
                if(version==1){
                    throw CartCrashedException(/*x*/nextCart.x, /*y*/nextCart.y);
                } else if (version == 2){
                    crashed = true;
                    // remove crashed car and continue wihth next loop
                    removeCrashed(carts);
                    continue;
                }
            }       

            // update carts matrix and their decision
            // Carts update
            carts.coeffRef(originalCart.y, originalCart.x) = static_cast<int>(Cart::None);
            carts.coeffRef(nextCart.y, nextCart.x) = static_cast<int>(nextCart.cart);
            // decision update
            if (nextTrack.track == Track::Plus){
                Decision nextDecision =  incDecision(previousCartDecision);
                // cartsDecisions.coeffRef(originalCart.y, originalCart.x) has become irrelevant and can be overwritten
                cartsDecisions.coeffRef(nextCart.y, nextCart.x) = static_cast<int>(nextDecision);
                std::cout << "set next decision for (" << nextCart.x << ", " << nextCart.y << ") from " << static_cast<int>(carts.coeffRef(originalCart.y, originalCart.x)) << " to " << static_cast<int>(nextDecision) << "\t" <<  std::flush;
            } else {
                // Need to carry over decision information
                cartsDecisions.coeffRef(nextCart.y, nextCart.x) = static_cast<int>(previousCartDecision);
            }

        }
    std::cout << std::endl;
    
}

SMatrix generateInitialDecisionMatrix(size_t x, size_t y, SMatrix & carts){
    SMatrix decs; decs.resize(y,x);
    for (int k=0; k<carts.outerSize(); ++k){
        for (SMatrix::InnerIterator cart(carts,k); cart; ++cart){
            if(cart.value()!=static_cast<int>(Cart::None)){
                decs.coeffRef(cart.row(), cart.col()) = static_cast<int>(Decision::left);
            }
        }
    }
    return decs;
}

SMatrix replaceCartsWithTracks(SMatrix carts, SMatrix input){
    SMatrix rep = SMatrix(input); // copy
    for (int k=0; k<carts.outerSize(); ++k){
        for (SMatrix::InnerIterator cart(carts,k); cart; ++cart){
            if(cart.value() == static_cast<int>(Cart::Left) || cart.value() == static_cast<int>(Cart::Right)){
                rep.coeffRef(cart.row(), cart.col()) = static_cast<int>(Track::Minus);
            }
            else if (cart.value() ==static_cast<int>(Cart::Up) || cart.value()==static_cast<int>(Cart::Down)){
                rep.coeffRef(cart.row(), cart.col()) = static_cast<int>(Track::Pipe);
            }
            else if (cart.value() == static_cast<int>(Cart::Crashed)){
                assert(0 && "Initial input is not supposed to contain crashed carts");
            }
            else {
                std::cout << "WAIT.... this iteration contains 0 values?" << std::endl;
            }
        }
    }
    return rep;
}

template <typename T>
using PMatrix = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>; 

char printTrackMap(int track){

    std::map<Track, char> trackMap;
    trackMap.insert(std::make_pair(Track::None, ' '));
    trackMap.insert(std::make_pair(Track::Slash, '/'));
    trackMap.insert(std::make_pair(Track::Backslash, '\\'));
    trackMap.insert(std::make_pair(Track::Minus, '-'));
    trackMap.insert(std::make_pair(Track::Plus, '+'));
    trackMap.insert(std::make_pair(Track::Pipe, '|'));

    return std::get<1>(*trackMap.find(static_cast<Track>(track)));
}

char printCartMap(int cart){

    std::map<Cart, char> cartMap;
    cartMap.insert(std::make_pair(Cart::None, '?'));
    cartMap.insert(std::make_pair(Cart::Up, '^'));
    cartMap.insert(std::make_pair(Cart::Down, 'v'));
    cartMap.insert(std::make_pair(Cart::Left, '<'));
    cartMap.insert(std::make_pair(Cart::Right, '>'));
    cartMap.insert(std::make_pair(Cart::Crashed, 'X'));

    return std::get<1>(*cartMap.find(static_cast<Cart>(cart)));
}

void visualize(SMatrix & tracks, SMatrix carts){

    PMatrix<int> trackz = PMatrix<int>(tracks); // copy
    PMatrix<char> trackc = trackz.unaryExpr(&printTrackMap);

}

int main(int argc, char* argv[]) { 
    std::string filename = "input1.txt";
    // actually when counting from 1! I.e. the numbers that vim displays at G$
    long unsigned int x=13;
    long unsigned int y=6;
    if (argc >= 4){
        filename = std::string(argv[1]);
        x = atoi(argv[2]);
        y = atoi(argv[3]);
    }
    std::cout << "File: " << filename << std::endl << std::endl;
    // parse carts
    std::cout << std::endl << "carts:" << std::endl;
    std::string input = readFileToString(filename);
    std::cout << "." << std::flush;
    SMatrix carts = parseCartsPositions(input, x,y); 
    std::cout << "." << std::flush;
    // parse tracks
    std::cout << carts << std::endl << std::endl << "tracks:" << std::endl;
    SMatrix tracks = parseTracks(input, x,y);
    // replace carts in input SMatrix with straight tracks
    SMatrix repTracks = replaceCartsWithTracks(carts, tracks);
    std::cout << tracks << std::endl;

    // another SMatrix which contains info about the next intersection turn
    SMatrix cartDecisions = generateInitialDecisionMatrix(x,y,carts);

    // task 1:
    bool doTask2 = true;

    try{
        while(1)
            moveCarts(carts, repTracks, cartDecisions, 1);
    } catch (CartCrashedException& e){
        std::cout << std::endl << "Crash at (" << e.m_x << ", " << e.m_y << ")!" << std::endl;
    }


    // task 2:
    while(doTask2){
        // remove the crashed carts
        removeCrashed(carts);

        // check if only one cart is left
        if(isLessThanTwoCartsLeft(carts)){
            std::cout << "Fini: " << anyCartsCoordsAsString(carts) << std::endl;
            break;
        }
        // else loop
        try{
            moveCarts(carts, repTracks, cartDecisions,2);
        } catch (CartCrashedException& e){
            std::cout << std::endl << "Crash at (" << e.m_x << ", " << e.m_y << ")!" << std::endl;
        }
    }

}
