/* -*- Mode:C++; c-file-style:"gnu"; indent-tabs-mode:nil; -*- */

//////////////////////////////////// 
//author: Xiaohui, Zhou
//ID: 104-014-248

//debug mode, de-comment to enable it
#define DEBUG

#ifdef DEBUG
#define DE(x); x
#else
#define DE(x); 
#endif

//change to 1 to turn day saving time on when calculating time difference
#define DST   0 
/////////////////////////////////////
#include <boost/lexical_cast.hpp>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <iostream>
#include <map>
#include <string>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctime>
#include <fcntl.h>
#include <pthread.h>
#include "http-headers.h"
#include "http-request.h"
#include "http-response.h"
#include "compat.h"

using namespace std;

#define PROXY_LISTENING_PORT_NUM        14886
#define PROXY_ASKING_PORT_NUM           80
#define MAX_SERVER_CONNECTION           100
#define MAX_CLIENT_CONNECTION           20
#define BUFFER_SIZE                     1024

typedef map<string,string> cache_map;

struct cache_struct{
  int client_fd;
  pthread_mutex_t * mutex;
  cache_map * cache;
};

int connected_server_num;
int connected_client_num;
 
/*******************************************************************************
 * @brief   Create a new socket for connecting to server
 * @returns the file descriptor for this new socket, 
 *          or <0 if error occurs.
 */
int createSocketToServer(const char* host, unsigned short port){
  int portNum = port;
  
  //socket
  int socketFd = socket(AF_INET,SOCK_STREAM, IPPROTO_TCP);
  if(socketFd < 0){
    perror("Create socket failed..");
    return -1;
  }

  //sockaddr
  struct sockaddr_in sockAddr;
  memset(&sockAddr, 0, sizeof(sockAddr));
  sockAddr.sin_family = AF_INET;
  sockAddr.sin_port   = htons(portNum);

  int res = inet_pton(AF_INET,host,&sockAddr.sin_addr);
  if(-1 == res){
    perror("Invalid address family");
    return -1;
  }else if( 0 == res){
    perror("Invalid IP address");
    return -2;
  }
  
  //connect
  if(-1 == connect(socketFd, (struct sockaddr *)&sockAddr, sizeof(sockAddr))){
    perror("DEBUG::62:connect failed..");
    return -3;
  }  

  return socketFd;
  
}

/*******************************************************************************
 * @brief   read packets and sent packets with client
 *
 */
void interact_with_client(int clientFd,pthread_mutex_t *mutex, cache_map *cache){
  
  while(true)
  { 
    string message_from_client = "";
    char*  message_to_server = NULL;
    size_t message_to_server_length = 0;
    string message_from_server = "";//also the message to client
    string error_message_to_client;
    
    string cached_string;
    bool useCache =false;
    bool check_modified = false;

    HttpRequest request_from_client;
    HttpHeaders server_response;
	
    unsigned int headers_length;
    unsigned int real_headers_length;
    unsigned int content_length = 0;
    unsigned int total_length;
    char* header_buf = NULL;
    const char* buf = NULL;


    //load buffer from client
    while(NULL == memmem(message_from_client.c_str(), message_from_client.length(),
                         "\r\n\r\n" , 4)){
      char temp_buffer[BUFFER_SIZE];
      if(0 > read(clientFd,temp_buffer,BUFFER_SIZE* sizeof(char))){
        perror("failed to read the message from client.");
        return;
      }
      message_from_client.append(temp_buffer);
    }
    
    DE(cout<< "DEBUG::message from client:\n" << message_from_client << "-----\n";);
    
    //parse buffer from client
    try{
      request_from_client.ParseRequest(message_from_client.c_str(),message_from_client.length());
    }
    catch(ParseException exception){
      perror("exception raised when parse message from client..");
      if(std::string::npos != message_from_client.find("HTTP/1.1")){
        if(strcmp(exception.what(),"Request is not GET"))
          error_message_to_client = "HTTP/1.1 501 Not Implemented\r\n\r\n";
        else
          error_message_to_client = "HTTP/1.1 400 Bad Request\r\n\r\n";
      }else{
        if(strcmp(exception.what(),"Request is not GET"))
          error_message_to_client = "HTTP/1.0 501 Not Implemented\r\n\r\n";
        else
          error_message_to_client = "HTTP/1.0 400 Bad Request\r\n\r\n"; 
      }
      if(0 > write(clientFd, error_message_to_client.c_str(), error_message_to_client.length())){
        perror ("failed to send error message to client..");
        connected_client_num --;
        close(clientFd);
        return;
      }        
    }

    //check if the request was cached
    string request_path = request_from_client.GetHost() + request_from_client.GetPath();
    cache_map::iterator it = cache->find(request_path);
    if(it != cache->end()){
      //cached, check if expired
      cached_string = it->second;
      //if no "expire" field, assume it will never expire
      if(string::npos == cached_string.find("Expires:")){
        DE(cout 
           << "***************************************\n"
           << "     EXPIRE not found, Not Expired\n" 
           << "***************************************\n";);
        useCache = true;
        goto SEND_TO_CLIENT;
      }
      
      HttpResponse header;
      string expire_time;
      string last_modified_time;
      header.ParseResponse(it->second.c_str() , it->second.length());
      expire_time         =  header.FindHeader("Expires");
      last_modified_time  =  header.FindHeader("Last-Modified");

      time_t current_time_t;
      time_t expire_time_t;
      time(&current_time_t);

      struct tm exp_gmtime_tm;
      struct tm * cur_gmtime_tm;
      
      cur_gmtime_tm = gmtime(&current_time_t);
      
      if( NULL == strptime(expire_time.c_str(), "%a, %d %b %Y %H:%M:%S", &exp_gmtime_tm)){
        perror("failed to parse expire_time string..");
        goto CONNECT_TO_SERVER;  
      }
      
      //currently we are not using day saving time (2014-2-16)
      exp_gmtime_tm.tm_isdst = DST;
   
      expire_time_t = mktime(&exp_gmtime_tm);

      if (difftime(current_time_t,expire_time_t) > 0){
        DE(cout 
           << "********************************************\n"
           << "diff: now - expire in sec: "<< difftime(current_time_t,expire_time_t) << endl
           << "now:    " << asctime(cur_gmtime_tm)
           << "expire: " << expire_time << endl
           << "   Expired, need to check if updated\n" 
           << "********************************************\n";);
        
        request_from_client.AddHeader("If-Modified-Since",last_modified_time);
        check_modified = true;
        goto CONNECT_TO_SERVER;  
      } else {
        DE(cout 
           << "********************************************\n"
           << "diff: now - expire in sec: "<< difftime(current_time_t,expire_time_t) << endl
           << "now:    " << asctime(cur_gmtime_tm)
           << "expire: " << expire_time << endl
           << "            Not  Expired\n" 
           << "********************************************\n";);
        useCache = true;
        goto SEND_TO_CLIENT;
      }
    }else{
    CONNECT_TO_SERVER:
      // not cached or expired
      int serverFd = createSocketToServer(request_from_client.GetHost().c_str(),
                                          request_from_client.GetPort());
      if( 0 > serverFd){
        //Failed to connect to the server
        error_message_to_client = "HTTP/1.1 404 Server Not Found\r\n\r\n";
        if(0 > write(clientFd, error_message_to_client.c_str(), 
                     error_message_to_client.length())){
          perror ("failed to send error message to client..");
        }
        close(clientFd);
        return;
      }
      
      connected_server_num ++;
      //create the message to server
      message_to_server_length = request_from_client.GetTotalLength() + 1;
      message_to_server = (char *) malloc(message_to_server_length);
      request_from_client.FormatRequest(message_to_server);
      DE(cout << "Message to server:\n" << message_to_server <<"\n-----\n";);
      
      //send message to server
      if( 0 > write(serverFd, message_to_server, message_to_server_length)){
        perror("failed to send message to server.Use cache if available..");
        free(message_to_server);
        close(serverFd);
        connected_server_num --;
        useCache = true;
        goto SEND_TO_CLIENT;
      }
      
      DE(cout << "Finish send message to server.\n";);
      free(message_to_server);

      //receive message from server
      fcntl(serverFd, F_SETFL, O_NONBLOCK);
      time_t begin, end;
   
      //header
      while(true){      
        time(&begin);
        time(&end);
        char temp_buffer[BUFFER_SIZE];
        int read_result = read(serverFd,temp_buffer,sizeof(temp_buffer));
        
        //wait 10sec if error occurs

        while(0 > read_result){
          if (difftime(end,begin) > 10.0){
            perror("failed to read the message from server[1]. Use cache if available..");
            close(serverFd);
            connected_server_num --;
            useCache = true;
            goto SEND_TO_CLIENT;
          }
          time(&end);
          read_result = read(serverFd,temp_buffer,sizeof(temp_buffer));
        }
        
        //if finish reading everything
        if(read_result == 0){
          goto SEND_TO_CLIENT;
        }
        message_from_server.append(temp_buffer,read_result);
        
        //if finish reading header
        if(message_from_server.find("\r\n\r\n") != string::npos){
          break;
        }
      }
     	
      //content
      headers_length = message_from_server.find("\r\n\r\n") + 4;
      real_headers_length = headers_length;
      buf = message_from_server.c_str();
      header_buf = ((char *)memmem (message_from_server.c_str(),
                                    headers_length, "\r\n", 2)) + 2;
      for(int i = 0; *(buf + i) != '\r' && *(buf + i +1) != '\n'; i++)
        headers_length--;
      headers_length -= 2;
      server_response.ParseHeaders(header_buf, headers_length);
      //in case using different headed name
      content_length = atoi(server_response.FindHeader("Content-Length").c_str());
      if(content_length == 0)
        content_length = atoi(server_response.FindHeader("Content-length").c_str());
   
      total_length = real_headers_length + content_length;
      DE(
         cout << "header length: " << real_headers_length
         << "\n content length: " << content_length
         << "\n total length: "  << total_length
         << endl;
         );
      
      while(message_from_server.length() < total_length){      
        time(&begin);
        time(&end);
        char temp_buffer[BUFFER_SIZE];
        int read_result = read(serverFd,temp_buffer,sizeof(temp_buffer));
        //wait 10sec if error occurs
        while(0 > read_result){
          if (difftime(end,begin) > 10.0){
            perror("failed to read the message from server[2].Use cache if available..");
            close(serverFd);
            connected_server_num --;
            useCache = true;
            goto SEND_TO_CLIENT;
          }
          time(&end);
          read_result = read(serverFd,temp_buffer,sizeof(temp_buffer));
        }
        //if finish reading everything
        if(read_result == 0){
          goto SEND_TO_CLIENT;
        }
        message_from_server.append(temp_buffer,read_result);
      }

      //check if modified since ..
      if(check_modified){
        HttpResponse server_check_response;
        server_check_response.ParseResponse(message_from_server.c_str(), 
                                            message_from_server.length());
        if(server_check_response.GetStatusCode().compare("304") == 0){
          useCache = true;
        }
      }	  
      close(serverFd);
      connected_server_num --;
      DE(cout << "Message from server:\n" << message_from_server 
         << "\n-------\nlength: " 
         << message_from_server.length() << endl;);
    }
    
  SEND_TO_CLIENT:
    if(useCache ==  true){
      DE(cout << "Message to client (from cache):\n" << cached_string 
         << "-----------\ntotal Length:" << cached_string.length() << endl;);
      if( 0 > write(clientFd, cached_string.c_str(), cached_string.length())){
        perror("failed to send message to client..");
        connected_client_num --;
        close(clientFd);
        return;
      }
    }else{//use response from server

      //add the message from server to cache
      pthread_mutex_lock(mutex);
      //delete the old one if it exists
      if(cache->find(request_path) != cache->end())
        cache->erase(request_path);
      //add the new one
      cache->insert(pair<string,string>(request_path,message_from_server));
      pthread_mutex_unlock(mutex);
      
      DE(cout << "Message to client (from server):\n" << message_from_server 
         << "\n-----------\ntotal Length:" << message_from_server.length() << endl;);
      if( 0 > write(clientFd, message_from_server.c_str(), message_from_server.length())){
        perror("failed to send message to client..");
        connected_client_num --;
        close(clientFd);
        return;
      }
    }
    
    DE( cout << "DEBUG::finished send message to client\n";);
    
    if(request_from_client.FindHeader("Connection") == "close"){
      close(clientFd);
      connected_client_num --;
      DE(cout << "connection to client closed.\n";);
      return;
    }
  }
}

//function used for pthread to handle request from client
void *pthread_interact_with_client (void *structure){
  cache_struct * cache_structure = (cache_struct *) structure;
  interact_with_client(cache_structure->client_fd, cache_structure->mutex, cache_structure->cache);
  free(cache_structure);
  return NULL;
}

/*******************************************************************************
  * Main function
 *
 */
int main (int argc, char *argv[]){
  
  signal(SIGPIPE, SIG_IGN);

  connected_server_num = 0;
  connected_client_num = 0;
  
  //Listening socket
  int ListeningSocketFd  = socket(AF_INET,SOCK_STREAM, IPPROTO_TCP);
  if(ListeningSocketFd < 0){
    perror("Create Listening socket failed..");
    return -1;
  }

  //sockaddr
  struct sockaddr_in sockAddr;
  memset(&sockAddr, 0, sizeof(sockAddr));
  sockAddr.sin_family = AF_INET;
  sockAddr.sin_port   = htons(PROXY_LISTENING_PORT_NUM);
  sockAddr.sin_addr.s_addr = htonl(INADDR_ANY);
  
  //bind
  if(-1 == bind (ListeningSocketFd, (struct sockaddr *)&sockAddr, sizeof(sockAddr))){
    perror("Listening socket Bind failed..");
    return -2;
  }

  //listen
  if(-1 == listen(ListeningSocketFd, MAX_CLIENT_CONNECTION)){
    perror("Listening socket listen failed..");
    return -3;
  }

  //cache
  cache_map cache;
  pthread_mutex_t cache_mutex;
  pthread_mutex_init(&cache_mutex, NULL);

  for(;;){
    DE(printf("DEBUG::proxy: start looping and ready to accept\n"););
    
    struct sockaddr clientAddr;
    socklen_t clientAddr_length;
  
    if(connected_client_num >= MAX_CLIENT_CONNECTION)
      continue;
    
    int clientFd = accept(ListeningSocketFd,&clientAddr,&clientAddr_length);
    
    if(0 > clientFd){
      perror ("accept client failed..");
      continue;
    }
    connected_client_num ++;

    //cache structure
    cache_struct *cache_structure = (cache_struct *)malloc (sizeof(struct cache_struct));
    cache_structure->client_fd = clientFd;
    cache_structure->mutex     = &cache_mutex;
    cache_structure->cache     = &cache;
    
    //use pthread to handle the request from client
    pthread_t pthread;
    pthread_create(&pthread, NULL, pthread_interact_with_client, (void *) cache_structure);
    pthread_detach(pthread);    
  }

  //never reached
  close(ListeningSocketFd);  
  return 0;
}
