#include "Common/parsing.hpp"
#include "Common/defs.hpp"
#include <iostream>
#include <vector>
#include <set>

template<class ValT>
class LinkedList {
    /** Private data structures **/
    struct Node {
        Node *mNextNode;
        Node *mPrevNode;
        ValT mVal;
        Node(ValT val) : mNextNode(nullptr), mPrevNode(nullptr), mVal(val) {}
    };
    Node *mHead;
    Node *mTail;
    u32 mSize;
public:
    /** Iterators **/
    struct Iterator {
        Node *node;
        Iterator(Node *pos) : node(pos) {}
        // Prefix increment
        const Iterator &operator++() {
            node = node->mNextNode;
            return *this;
        }
        // Postfix increment
        Iterator operator++(int) {
            Iterator itr = *this;
            ++(*this);
            return itr;
        }
        // Comparison
        bool operator==(const Iterator &itr) {
            return node == itr.node;
        }
        bool operator!=(const Iterator &itr) {
            return node != itr.node;
        }
    };
    Iterator begin()         { return Iterator(mHead);   }
    constexpr Iterator end() { return Iterator(nullptr); }

    /** Constructors and destructors **/
    LinkedList(ValT val) : mSize(1) {
        mHead = new Node(val);
        mTail = mHead;
    }
    ~LinkedList() {
        while(mHead != nullptr) {
            Node *tmpNode = mHead->mNextNode;
            delete mHead;
            mHead = tmpNode;
        }
    }

    /** Methods for manipulation **/
    u32 size() { return mSize; }

    ValT get_val(          Iterator itr) { return itr.node->mVal; }
    void set_val(ValT val, Iterator itr) { itr.node->mVal = val;  }

    // insert an element at the end of the list
    void push_back(ValT val) {
        Node *node = new Node(val);
        node->mPrevNode = mTail;
        mTail->mNextNode = node;
        mTail = node;
        mSize++;
    }

    // insert an element before the given node
    void insert(ValT val, Iterator itr) {
        Node *node = itr.node;
        Node *newNode = new Node(val);
        if (node == mHead)
            mHead = newNode;
        else
            node->mPrevNode->mNextNode = newNode;
        newNode->mPrevNode = node->mPrevNode;
        node->mPrevNode = newNode;
        newNode->mNextNode = node;
        mSize++;
    }
};

typedef u32 Stone;

u32 count_digits(u32 n) {
    u32 digits = 1;
    while (n > 9u) {
        n /= 10;
        ++digits;
    }
    return digits;
}

void blink(LinkedList<Stone> &stones) {
    for (auto itr = stones.begin(); itr != stones.end(); ++itr) {
        Stone val = stones.get_val(itr);
        // case 1.
        if (val == 0)
            stones.set_val(1, itr);
        else {
            u32 digits = count_digits(val);
            // case 2.
            if (!(digits & 1)) {
                Stone upper = val;
                for (st i = 0; i < digits / 2; ++i)
                    upper /= 10;
                Stone lower = upper;
                for (st i = 0; i < digits / 2; ++i)
                    lower *= 10;
                lower = val - lower;
                stones.insert(upper, itr);
                stones.set_val(lower, itr);
            }
            // case 3.
            else
                stones.set_val(val*2024u, itr);
        }
    }
}

void print(LinkedList<Stone> &stones) {
    for (auto itr = stones.begin(); itr != stones.end(); ++itr)
        std::cout << stones.get_val(itr) << " ";
    std::cout << std::endl;
}

int main() {
    /* PARSING */
    Parser in("in/11.in");
    LinkedList<Stone> stones(static_cast<Stone>(in.parse_int()));
    while (!in.is_eof()) {
        in.parse_char(' ');
        stones.push_back(static_cast<Stone>(in.parse_int()));
    }

    /* PART 1 */
    for (st i = 0; i < 25; ++i) {
        blink(stones);
        print(stones);
    }
    st sum = stones.size();
    std::cout << "part1: " << stones.size() << std::endl;

    /* PART 2 */
    sum = 0;
    std::cout << "part2: " << sum << std::endl;
}
