# ############################################### #
# Upload processed kaggle:social network data to  #
# mongodb@amazon.ec2                              #
# ############################################### #
import pymongo
import sys, os

## define environment class ClassName(object):
class G(object):
	""" Global varaibles: """
	ec2_ip       = "54.88.134.182"
	mongodb_port = 27017
	data_repo    = "/Users/beingzy/Documents/kaggle/socialNetwork/output/"
		

mongoClient = pymongo.MongoClient(G.ec2_ip, port = G.mongodb_port)
db          = mongoClient.kaggle_socialcircle
