#ifndef __TREE__
#define __TREE__

#include <iostream>
#include <vector>
#include <RcppArmadillo.h>


struct Node
{
    std::string node;
    double credit;
};
struct Edge
{
    Node *src;
    Node *dest;
    double probability;
};

// A class to represent a graph object
class Graph
{
public:
    // a vector of vectors to represent an adjacency list
    std::vector<Node> nodes;
    std::vector<std::vector<Edge>> adjList;

    int getNodeIndex(std::string nodeName)
    {
        int i = 0;
        for (auto &node : nodes)
        {

            if (node.node == nodeName)
            {
                break;
            }
            i++;
        }
        return (i);
    }

    Node *getNode(std::string nodeName)
    {
        for (auto &node : nodes)
        {
            if (node.node == nodeName)
            {
                return (&node);
            }
        }

        return (nullptr);
    }

    Graph() {}

    Graph(Rcpp::S4 turnModel, int state)
    {
        Rcpp::S4 graph;
        Rcpp::List rcppNodeList;
        Rcpp::List rcppEdgeList;

        if (state == 0)
        {
            graph = Rcpp::as<Rcpp::S4>(turnModel.slot("S0"));
            rcppNodeList = turnModel.slot("nodes.S0");
            rcppEdgeList = turnModel.slot("edges.S0");
        }
        else
        {
            graph = Rcpp::as<Rcpp::S4>(turnModel.slot("S1"));
            rcppNodeList = turnModel.slot("nodes.S1");
            rcppEdgeList = turnModel.slot("edges.S1");
        }

        for (int i = 0; i < rcppNodeList.size(); i++)
        {
            Node n = {Rcpp::as<std::string>(rcppNodeList[i]), 0};
            nodes.push_back(n);
        }
        int N = nodes.size();
        adjList.resize(N);
        for (int i = 0; i < rcppEdgeList.size(); i++)
        {
            Rcpp::S4 edge = rcppEdgeList[i];
            SEXP edgeVec = edge.slot("edge");
            Rcpp::StringVector vec(edgeVec);
            SEXP prob = edge.slot("prob");
            Rcpp::NumericVector probVec(prob);

            //Rcpp::Rcout <<"vec="<<vec << ", probVec=" << probVec<<std::endl;
            Node *src = getNode(Rcpp::as<std::string>(vec[0]));
            Node *dest = getNode(Rcpp::as<std::string>(vec[1]));
            Edge e = {src, dest, probVec[0]};
            int nodeIndex = getNodeIndex(src->node);
            adjList[nodeIndex].push_back(e);
        }
    }

    Edge getEdge(std::string src, std::string dest)
    {
        int nodeIndex = getNodeIndex(src);
        std::vector<Edge> edges = adjList[nodeIndex];
        for (auto &edge : edges)
        {
            if (edge.dest->node == dest)
            {
                return (edge);
            }
        }
        return Edge();
    }

    std::vector<Edge>* getOutgoingEdges(std::string src)
    {
        int nodeIndex = getNodeIndex(src);

        std::vector<Edge>* edges = &adjList[nodeIndex];
        return (edges);
    }
    
    void printGraph()
    {
        int N = nodes.size();
        for (int i = 0; i < N; i++)
        {
            // print the current vertex number
            std::cout << "i=" << i << ", " << nodes[i].node << " ——> ";

            // print all neighboring vertices of a vertex `i`
            for (auto v : adjList[i])
            {
                std::cout << v.dest->node << " ";
            }
            std::cout << std::endl;
        }
    }
    
    void updateEdgeProbs()
    {
        for (auto &node : nodes)
        {
            std::vector<Edge>* outgoingEdges = getOutgoingEdges(node.node);
            if (!outgoingEdges->empty())
            {
                std::vector<double> edgeCredits;
                Rcpp::StringVector nodeNames;
                for (auto it = outgoingEdges->begin(); it != outgoingEdges->end(); ++it)
                {
                    edgeCredits.push_back((*it).dest->credit);
                    nodeNames.push_back((*it).dest->node);
                }
                arma::vec v(edgeCredits);
                //Rcpp::Rcout << "nodeNames=" << nodeNames << std::endl;
                //Rcpp::Rcout << "outgoingEdges=" << outgoingEdges<< std::endl;
                //Rcpp::Rcout << "v=" << v << std::endl;
                double m = arma::max(v);
                v = exp(v - m);
                // //Rcpp::Rcout << "m=" << m<< std::endl;
                double exp_sum = arma::accu(v);
                arma::vec probVec = v / exp_sum;
                int i=0;
                for (auto it = outgoingEdges->begin(); it != outgoingEdges->end(); ++it)
                {
                    (*it).probability = probVec[i];
                    i++;
                }
                
            }
        }
    }
};

#endif