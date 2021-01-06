#ifndef __TREE__
#define __TREE__

#include <iostream>
#include <vector>
#include <memory>

struct TreeNode
{
    // member vars
    std::string turn;
    double qval;
    double etrace;
    std::vector<std::weak_ptr<TreeNode>> siblings;
    std::vector<std::shared_ptr<TreeNode>> child; 
    
};

std::shared_ptr<TreeNode> newNode(std::string turn);
std::shared_ptr<TreeNode> initS1();
std::shared_ptr<TreeNode> initS2();
bool isChild(std::shared_ptr<TreeNode> currentNode, std::string newNode);
std::shared_ptr<TreeNode> getChildNode(std::shared_ptr<TreeNode> currentNode, std::string newNode);
std::vector<double> getSiblingQvals(std::shared_ptr<TreeNode> currentNode);



#endif