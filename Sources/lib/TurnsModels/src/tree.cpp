#include <Rcpp.h>
#include "tree.hpp"

std::shared_ptr<TreeNode> newNode(std::string turn)
{
    auto temp = std::make_shared<TreeNode>();
    temp->turn = turn;
    temp->credit=0;
    //Rcpp::Rcout <<"Creating node ="<<turn<<std::endl;
    return temp;
}

std::shared_ptr<TreeNode> newNode2(std::string turn,double probability)
{
    auto temp = std::make_shared<TreeNode>();
    temp->turn = turn;
    temp->credit=probability;
    //Rcpp::Rcout <<"Creating node ="<<turn<<std::endl;
    return temp;
}

bool isChild(std::shared_ptr<TreeNode> currentNode, std::string newNode)
{
    bool foundChild = false;
    for (auto i = currentNode->child.begin(); i != currentNode->child.end(); i++)
    {
        if ((*i)->turn == newNode)
        {
            foundChild = true;
            break;
        }
    }
    return (foundChild);
}

std::shared_ptr<TreeNode> getChildNode(std::shared_ptr<TreeNode> currentNode, std::string newNode)
{
    //Rcpp::Rcout <<"Getting child nodes of currentNode= "<<currentNode->turn<<std::endl;
    std::shared_ptr<TreeNode> child;
    for (auto i = currentNode->child.begin(); i != currentNode->child.end(); i++)
    {
        //Rcpp::Rcout <<"child node = "<< (*i)->turn<<std::endl;
        if ((*i)->turn == newNode)
        {
            child = *i;
            break;
        }
    }
        
    return (child);
}

std::vector<double> getSiblingCredits(std::shared_ptr<TreeNode> currentNode)
{
    std::vector<double> credits;
    for (auto i = currentNode->siblings.begin(); i != currentNode->siblings.end(); i++)
    {
        std::shared_ptr<TreeNode> stgPtr = (*i).lock();
        credits.push_back(stgPtr->credit);
    }
    return (credits);
}



std::shared_ptr<TreeNode> getNode(std::shared_ptr<TreeNode> root, std::string nodeName)
{
    //Rcpp::Rcout << "td_error="<< td_error <<std::endl;  
    for (auto i = root->child.begin(); i != root->child.end(); i++)
    {
        if((*i)->turn == nodeName)
        {
            return(*i);
        }
        //Rcpp::Rcout <<  "turn="<< (*i)->turn << ", qval=" << (*i)->qval << ", etrace=" << (*i)->etrace<<std::endl;  
        std::vector<std::shared_ptr<TreeNode>> childNodes = (*i)->child;
        if (!childNodes.empty())
        {
             for (auto child = childNodes.begin(); child != childNodes.end(); child++)
             {
                if((*child)->turn == nodeName)
                {
                    return(*child);
                }
                
             }
        }  
    }
    
}

std::shared_ptr<TreeNode> initS1()
{
    std::shared_ptr<TreeNode> root = newNode("E");
    std::shared_ptr<TreeNode> dcb = newNode("dcb");
    std::shared_ptr<TreeNode> dch = newNode("dch");
    std::shared_ptr<TreeNode> gak = newNode("gak");
    std::shared_ptr<TreeNode> gab = newNode("gab");
    std::shared_ptr<TreeNode> bak = newNode("bak");
    std::shared_ptr<TreeNode> bag = newNode("bag");
    std::shared_ptr<TreeNode> bcd = newNode("bcd");
    std::shared_ptr<TreeNode> bch = newNode("bch");

    (root->child).push_back(dcb);
    (root->child).push_back(dch);
    (root->child).push_back(gak);
    (root->child).push_back(gab);
    (root->child[0]->child).push_back(bak);
    (root->child[0]->child).push_back(bag);
    (root->child[3]->child).push_back(bcd);
    (root->child[3]->child).push_back(bch);

    //dcb->siblings.assign(dch,gak,gab);
    dcb->siblings.push_back(dch);
    dcb->siblings.push_back(gak);
    dcb->siblings.push_back(gab);

    //dch->siblings.assign(dcb,gak,gab);
    dch->siblings.push_back(dcb);
    dch->siblings.push_back(gak);
    dch->siblings.push_back(gab);

    //gak->siblings.assign(dch,dcb,gab);
    gak->siblings.push_back(dch);
    gak->siblings.push_back(dcb);
    gak->siblings.push_back(gab);

    //gab->siblings.assign(dch,gak,dcb);
    gab->siblings.push_back(dch);
    gab->siblings.push_back(gak);
    gab->siblings.push_back(dcb);

    bak->siblings.push_back(bag);
    bag->siblings.push_back(bak);

    bcd->siblings.push_back(bch);
    bch->siblings.push_back(bcd);

    return (root);
}

std::shared_ptr<TreeNode> initS2()
{

    std::shared_ptr<TreeNode> root = newNode("I");
    std::shared_ptr<TreeNode> hcb = newNode("hcb");
    std::shared_ptr<TreeNode> hcd = newNode("hcd");
    std::shared_ptr<TreeNode> kag = newNode("kag");
    std::shared_ptr<TreeNode> kab = newNode("kab");
    std::shared_ptr<TreeNode> bak = newNode("bak");
    std::shared_ptr<TreeNode> bag = newNode("bag");
    std::shared_ptr<TreeNode> bcd = newNode("bcd");
    std::shared_ptr<TreeNode> bch = newNode("bch");

    (root->child).push_back(hcb);
    (root->child).push_back(hcd);
    (root->child).push_back(kag);
    (root->child).push_back(kab);

    (root->child[0]->child).push_back(bak);
    (root->child[0]->child).push_back(bag);
    (root->child[3]->child).push_back(bcd);
    (root->child[3]->child).push_back(bch);

    //hcb->siblings.assign(hcd,kag,kab);
    hcb->siblings.push_back(hcd);
    hcb->siblings.push_back(kag);
    hcb->siblings.push_back(kab);

    //hcd->siblings.assign(hcb,kag,kab);
    hcd->siblings.push_back(hcb);
    hcd->siblings.push_back(kag);
    hcd->siblings.push_back(kab);

    //kag->siblings.assign(hcb,hcd,kab);
    kag->siblings.push_back(hcb);
    kag->siblings.push_back(hcd);
    kag->siblings.push_back(kab);

    //kab->siblings.assign(hcb,hcd,kag);
    kab->siblings.push_back(hcb);
    kab->siblings.push_back(hcd);
    kab->siblings.push_back(kag);

    bak->siblings.push_back(bag);
    bag->siblings.push_back(bak);

    bcd->siblings.push_back(bch);
    bch->siblings.push_back(bcd);

    return (root);
}


std::shared_ptr<TreeNode> probInitS1()
{
    std::shared_ptr<TreeNode> root = newNode("E");
    std::shared_ptr<TreeNode> dcb = newNode2("dcb",0.25);
    std::shared_ptr<TreeNode> dch = newNode2("dch",0.25);
    std::shared_ptr<TreeNode> gak = newNode2("gak",0.25);
    std::shared_ptr<TreeNode> gab = newNode2("gab",0.25);
    std::shared_ptr<TreeNode> bak = newNode2("bak",0.5);
    std::shared_ptr<TreeNode> bag = newNode2("bag",0.5);
    std::shared_ptr<TreeNode> bcd = newNode2("bcd",0.5);
    std::shared_ptr<TreeNode> bch = newNode2("bch",0.5);

    (root->child).push_back(dcb);
    (root->child).push_back(dch);
    (root->child).push_back(gak);
    (root->child).push_back(gab);
    (root->child[0]->child).push_back(bak);
    (root->child[0]->child).push_back(bag);
    (root->child[3]->child).push_back(bcd);
    (root->child[3]->child).push_back(bch);

    //dcb->siblings.assign(dch,gak,gab);
    dcb->siblings.push_back(dch);
    dcb->siblings.push_back(gak);
    dcb->siblings.push_back(gab);

    //dch->siblings.assign(dcb,gak,gab);
    dch->siblings.push_back(dcb);
    dch->siblings.push_back(gak);
    dch->siblings.push_back(gab);

    //gak->siblings.assign(dch,dcb,gab);
    gak->siblings.push_back(dch);
    gak->siblings.push_back(dcb);
    gak->siblings.push_back(gab);

    //gab->siblings.assign(dch,gak,dcb);
    gab->siblings.push_back(dch);
    gab->siblings.push_back(gak);
    gab->siblings.push_back(dcb);

    bak->siblings.push_back(bag);
    bag->siblings.push_back(bak);

    bcd->siblings.push_back(bch);
    bch->siblings.push_back(bcd);

    return (root);
}

std::shared_ptr<TreeNode> probInitS2()
{

    std::shared_ptr<TreeNode> root = newNode("I");
    std::shared_ptr<TreeNode> hcb = newNode2("hcb",0.25);
    std::shared_ptr<TreeNode> hcd = newNode2("hcd",0.25);
    std::shared_ptr<TreeNode> kag = newNode2("kag",0.25);
    std::shared_ptr<TreeNode> kab = newNode2("kab",0.25);
    std::shared_ptr<TreeNode> bak = newNode2("bak",0.5);
    std::shared_ptr<TreeNode> bag = newNode2("bag",0.5);
    std::shared_ptr<TreeNode> bcd = newNode2("bcd",0.5);
    std::shared_ptr<TreeNode> bch = newNode2("bch",0.5);

    (root->child).push_back(hcb);
    (root->child).push_back(hcd);
    (root->child).push_back(kag);
    (root->child).push_back(kab);

    (root->child[0]->child).push_back(bak);
    (root->child[0]->child).push_back(bag);
    (root->child[3]->child).push_back(bcd);
    (root->child[3]->child).push_back(bch);

    //hcb->siblings.assign(hcd,kag,kab);
    hcb->siblings.push_back(hcd);
    hcb->siblings.push_back(kag);
    hcb->siblings.push_back(kab);

    //hcd->siblings.assign(hcb,kag,kab);
    hcd->siblings.push_back(hcb);
    hcd->siblings.push_back(kag);
    hcd->siblings.push_back(kab);

    //kag->siblings.assign(hcb,hcd,kab);
    kag->siblings.push_back(hcb);
    kag->siblings.push_back(hcd);
    kag->siblings.push_back(kab);

    //kab->siblings.assign(hcb,hcd,kag);
    kab->siblings.push_back(hcb);
    kab->siblings.push_back(hcd);
    kab->siblings.push_back(kag);

    bak->siblings.push_back(bag);
    bag->siblings.push_back(bak);

    bcd->siblings.push_back(bch);
    bch->siblings.push_back(bcd);

    return (root);
}
