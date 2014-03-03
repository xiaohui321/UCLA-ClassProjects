////////////////////////////////////////////////////
// anim.cpp version 4.1
// Template code for drawing an articulated figure.
// CS 174A 
// Xiaohui, Zhou
////////////////////////////////////////////////////
#include <array>

#ifdef WIN32
#include <windows.h>
#include "GL/glew.h"
#include <GL/gl.h>
#include <GL/glu.h>
#else
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#ifdef WIN32
#include "GL/freeglut.h"
#else
#include <GLUT/glut.h>
#endif

#include "Ball.h"
#include "FrameSaver.h"
#include "Timer.h"
#include "Shapes.h"
#include "tga.h"

#include "Angel/Angel.h"

#ifdef __APPLE__
#define glutInitContextVersion(a,b)
#define glutInitContextProfile(a)
#define glewExperimental int glewExperimentalAPPLE
#define glewInit()
#endif

FrameSaver FrSaver ;
Timer TM ;

BallData *Arcball = NULL ;
int Width = 500;
int Height = 500 ;
int Button = -1 ;
float Zoom = 1 ;
int PrevY = 0 ;

int Animate = 0 ;
int Recording = 0 ;

void resetArcball() ;
void save_image();
void instructions();
void set_colour(float r, float g, float b) ;

const int STRLEN = 100;
typedef char STR[STRLEN];

#define PI 3.1415926535897
#define X 0
#define Y 1
#define Z 2

//texture
//GLuint texture_ground_view;

GLuint texture_plane;
GLuint texture_tank;
GLuint texture_flag;
GLuint texture_house;
GLuint texture_house_roof;


// Structs that hold the Vertex Array Object index and number of vertices of each shape.
ShapeData cubeData;
ShapeData sphereData;
ShapeData coneData;
ShapeData cylData;
ShapeData TriData;

// Matrix stack that can be used to push and pop the modelview matrix.
class MatrixStack {
    int    _index;
    int    _size;
    mat4*  _matrices;

   public:
    MatrixStack( int numMatrices = 32 ):_index(0), _size(numMatrices)
        { _matrices = new mat4[numMatrices]; }

    ~MatrixStack()
	{ delete[]_matrices; }

    void push( const mat4& m ) {
        assert( _index + 1 < _size );
        _matrices[_index++] = m;
    }

    mat4& pop( void ) {
        assert( _index - 1 >= 0 );
        _index--;
        return _matrices[_index];
    }
};

MatrixStack  mvstack;
mat4         model_view;
GLint        uModelView, uProjection, uView;
GLint        uAmbient, uDiffuse, uSpecular, uLightPos, uShininess;
GLint        uTex, uEnableTex;

// The eye point and look-at point.
// Currently unused. Use to control a camera with LookAt().
Angel::vec4 eye{0, 0.0, 50.0,1.0};
Angel::vec4 ref{0.0, 0.0, 0.0,1.0};
Angel::vec4 up{0.0,1.0,0.0,0.0};

double TIME = 0.0 ;

/////////////////////////////////////////////////////
//    PROC: printFPS()
//    DOES: prints frames per second on the console
//////////////////////////////////////////////////////
double old_time = 0.0;
int frame_num = 0;
int fps = 0;

void printFPS(){
	frame_num++;
	double current_time  = TM.GetElapsedTime();
	double time_interval = current_time - old_time;

	if (time_interval > 1.0){
		fps = frame_num / time_interval;
		old_time = current_time;
		frame_num = 0;
		printf("FPS = %i\n", fps);
	}

}

/////////////////////////////////////////////////////
//    PROC: drawCylinder()
//    DOES: this function 
//          render a solid cylinder  oriented along the Z axis. Both bases are of radius 1. 
//          The bases of the cylinder are placed at Z = 0, and at Z = 1.
//
//          
// Don't change.
//////////////////////////////////////////////////////
void drawCylinder(void)
{
    glUniformMatrix4fv( uModelView, 1, GL_TRUE, model_view );
    glBindVertexArray( cylData.vao );
    glDrawArrays( GL_TRIANGLES, 0, cylData.numVertices );
}

//////////////////////////////////////////////////////
//    PROC: drawCone()
//    DOES: this function 
//          render a solid cone oriented along the Z axis with base radius 1. 
//          The base of the cone is placed at Z = 0, and the top at Z = 1. 
//         
// Don't change.
//////////////////////////////////////////////////////
void drawCone(void)
{
    glUniformMatrix4fv( uModelView, 1, GL_TRUE, model_view );
    glBindVertexArray( coneData.vao );
    glDrawArrays( GL_TRIANGLES, 0, coneData.numVertices );
}


//////////////////////////////////////////////////////
//    PROC: drawCube()
//    DOES: this function draws a cube with dimensions 1,1,1
//          centered around the origin.
// 
// Don't change.
//////////////////////////////////////////////////////

void drawCube(void)
{
    //glBindTexture( GL_TEXTURE_2D, texture_cube );
    //glUniform1i( uEnableTex, 1 );
    glUniformMatrix4fv( uModelView, 1, GL_TRUE, model_view );
    glBindVertexArray( cubeData.vao );
    glDrawArrays( GL_TRIANGLES, 0, cubeData.numVertices );
    //glUniform1i( uEnableTex, 0 );
}


//////////////////////////////////////////////////////
//    PROC: drawSphere()
//    DOES: this function draws a sphere with radius 1
//          centered around the origin.
// 
// Don't change.
//////////////////////////////////////////////////////

void drawSphere(void)
{
    //glBindTexture( GL_TEXTURE_2D, texture_plane1);
    //glUniform1i( uEnableTex, 1);
    glUniformMatrix4fv( uModelView, 1, GL_TRUE, model_view );
    glBindVertexArray( sphereData.vao );
    glDrawArrays( GL_TRIANGLES, 0, sphereData.numVertices );
    //glUniform1i( uEnableTex, 0 );
}

//new added
void drawTri(void){
	glUniformMatrix4fv(uModelView, 1, GL_TRUE, model_view);
	glBindVertexArray(TriData.vao);
	glDrawArrays(GL_TRIANGLES, 0, TriData.numVertices);
}
void resetArcball()
{
    Ball_Init(Arcball);
    Ball_Place(Arcball,qOne,0.75);
}


//////////////////////////////////////////////////////
//    PROC: myKey()
//    DOES: this function gets caled for any keypresses
// 
//////////////////////////////////////////////////////

void myKey(unsigned char key, int x, int y)
{
    float time ;
    switch (key) {
        case 'q':
        case 27:
            exit(0); 
        case 's':
            FrSaver.DumpPPM(Width,Height) ;
            break;
        case 'r':
            resetArcball() ;
            glutPostRedisplay() ;
            break ;
        case 'a': // togle animation
            Animate = 1 - Animate ;
            // reset the timer to point to the current time		
            time = TM.GetElapsedTime() ;
            TM.Reset() ;
            // printf("Elapsed time %f\n", time) ;
            break ;
        case '0':
            //reset your object
            break ;
        case 'm':
            if( Recording == 1 )
            {
                printf("Frame recording disabled.\n") ;
                Recording = 0 ;
            }
            else
            {
                printf("Frame recording enabled.\n") ;
                Recording = 1  ;
            }
            FrSaver.Toggle(Width);
            break ;
        case 'h':
        case '?':
            instructions();
            break;
    }
    glutPostRedisplay() ;

}

/*********************************************************
    PROC: myinit()
    DOES: performs most of the OpenGL intialization
     -- change these with care, if you must.

**********************************************************/

void myinit(void)
{
    // Load shaders and use the resulting shader program
    GLuint program = InitShader( "vshader.glsl", "fshader.glsl" );
    glUseProgram(program);

    // Generate vertex arrays for geometric shapes
    generateCube(program, &cubeData);
    generateSphere(program, &sphereData);
    generateCone(program, &coneData);
    generateCylinder(program, &cylData);
	generateTri(program, &TriData);

    uModelView  = glGetUniformLocation( program, "ModelView"  );
    uProjection = glGetUniformLocation( program, "Projection" );
    uView       = glGetUniformLocation( program, "View"       );

    glClearColor( 0.52, 0.81, 0.99, 1.0 ); // sky blue background

    uAmbient   = glGetUniformLocation( program, "AmbientProduct"  );
    uDiffuse   = glGetUniformLocation( program, "DiffuseProduct"  );
    uSpecular  = glGetUniformLocation( program, "SpecularProduct" );
    uLightPos  = glGetUniformLocation( program, "LightPosition"   );
    uShininess = glGetUniformLocation( program, "Shininess"       );
    uTex       = glGetUniformLocation( program, "Tex"             );
    uEnableTex = glGetUniformLocation( program, "EnableTex"       );

    glUniform4f(uAmbient,    0.2f,  0.2f,  0.2f, 1.0f);
    glUniform4f(uDiffuse,    0.6f,  0.6f,  0.6f, 1.0f);
    glUniform4f(uSpecular,   0.2f,  0.2f,  0.2f, 1.0f);
    glUniform4f(uLightPos,  15.0f, 15.0f, 30.0f, 0.0f);
    glUniform1f(uShininess, 100.0f);

    glEnable(GL_DEPTH_TEST);
	///////////////////////////////////////////////////////
    TgaImage plane;
	if (!plane.loadTGA("plane.tga"))
    {
        printf("Error loading image file\n");
        exit(1);
    }

	TgaImage tank;
	if (!tank.loadTGA("tank.tga"))
	{
		printf("Error loading image file\n");
		exit(1);
	}

	TgaImage flag;
	if (!flag.loadTGA("flag.tga"))
	{
		printf("Error loading image file\n");
		exit(1);
	}
	
	TgaImage house;
	if (!house.loadTGA("house.tga"))
	{
		printf("Error loading image file\n");
		exit(1);
	}

	TgaImage house_roof;
	if (!house_roof.loadTGA("houseRoof.tga"))
	{
		printf("Error loading image file\n");
		exit(1);
	}
	
	glGenTextures(1, &texture_plane);
	glBindTexture(GL_TEXTURE_2D, texture_plane);

	glTexImage2D(GL_TEXTURE_2D, 0, 4, plane.width, plane.height, 0,
		(plane.byteCount == 3) ? GL_BGR : GL_BGRA,
		GL_UNSIGNED_BYTE, plane.data);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	////////////////////////////////////////////////////////////////

	glGenTextures(1, &texture_tank);
	glBindTexture(GL_TEXTURE_2D, texture_tank);

	glTexImage2D(GL_TEXTURE_2D, 0, 4, tank.width, tank.height, 0,
		(tank.byteCount == 3) ? GL_BGR : GL_BGRA,
		GL_UNSIGNED_BYTE, tank.data);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    /////////////////////////////////////////////////////////////////////

	glGenTextures(1, &texture_flag);
	glBindTexture(GL_TEXTURE_2D, texture_flag);

	glTexImage2D(GL_TEXTURE_2D, 0, 4, flag.width, flag.height, 0,
		(flag.byteCount == 3) ? GL_BGR : GL_BGRA,
		GL_UNSIGNED_BYTE, flag.data);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	/////////////////////////////////////////////////////////////////////
	
	glGenTextures(1, &texture_house);
	glBindTexture(GL_TEXTURE_2D, texture_house);

	glTexImage2D(GL_TEXTURE_2D, 0, 4, house.width, house.height, 0,
		(house.byteCount == 3) ? GL_BGR : GL_BGRA,
		GL_UNSIGNED_BYTE, house.data);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	/////////////////////////////////////////////////////////////////////

	glGenTextures(1, &texture_house_roof);
	glBindTexture(GL_TEXTURE_2D, texture_house_roof);

	glTexImage2D(GL_TEXTURE_2D, 0, 4, house_roof.width, house_roof.height, 0,
		(house_roof.byteCount == 3) ? GL_BGR : GL_BGRA,
		GL_UNSIGNED_BYTE, house_roof.data);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	
	/////////////////////////////////////////////////////////////////////

    // Set texture sampler variable to texture unit 0
    // (set in glActiveTexture(GL_TEXTURE0))
    
    glUniform1i( uTex, 0);
    
    Arcball = new BallData;
    Ball_Init(Arcball);
    Ball_Place(Arcball,qOne,0.75);
}

/*********************************************************
    PROC: set_colour();
    DOES: sets all material properties to the given colour
    -- don't change
**********************************************************/

void set_colour(float r, float g, float b)
{
    float ambient  = 0.2f;
    float diffuse  = 0.6f;
    float specular = 0.2f;
    glUniform4f(uAmbient,  ambient*r,  ambient*g,  ambient*b,  1.0f);
    glUniform4f(uDiffuse,  diffuse*r,  diffuse*g,  diffuse*b,  1.0f);
    glUniform4f(uSpecular, specular*r, specular*g, specular*b, 1.0f);
}

/*********************************************************
**********************************************************
**********************************************************

    PROC: display()
    DOES: this gets called by the event handler to draw
          the scene, so this is where you need to build
          your ROBOT --  
      
        MAKE YOUR CHANGES AND ADDITIONS HERE

    Add other procedures if you like.

**********************************************************
**********************************************************
**********************************************************/
void resetView(){
	model_view = mat4(1.0f);
	model_view = LookAt(eye, ref, up);
	model_view *= Scale(Zoom);
}

void drawSun(){
	resetView();
	set_colour(1.0f, 0.0f, 0.0f);
	model_view *= Translate(3000.0f, 30.0f, 3000.0f);
	model_view *= Scale(50.0f);
	drawSphere();
}

void drawGround(){
	resetView();
	set_colour(0.15f, 0.5f, 0.15f);
	model_view *= Translate(0.0f, 0.0f, 0.0f);
	model_view *= Scale(1000.0f, 1.0f, 1000.0f);
	drawCube();
}

void drawHouse(int x, int y){
	resetView();
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, texture_house);
	glUniform1i(uEnableTex, 1);
	model_view *= Translate(x, 1.3f, y);
	model_view *= Scale(6.0f, 4.0f, 8.0f);
	drawCube();

	glBindTexture(GL_TEXTURE_2D, texture_house_roof);
	model_view *= Translate(0.0f, 1.0f, 0.0f);
	model_view *= Scale(1.1f, 1.1f, 1.1f);
	drawTri();

	model_view *= Translate(0.2f, 0.2f, 0.3f);
	model_view *= Scale(0.2f, 0.7f, 0.2f);
	drawCube();
	glUniform1i(uEnableTex, 0);
}
/*********************************************************
   Class tank
**********************************************************/
class tank
{
public:
	tank(int x, int z);
	void drawtank(bool move, bool spin);
	void destory(){ destroyed = true; }
private:
	int c_x, c_z;
	bool destroyed;
};
tank::tank(int x, int z){
	c_x = x;
	c_z = z;
	destroyed = false;
}
void tank::drawtank( bool move, bool spin){
	if (destroyed)
		return;
	resetView();
	model_view *= RotateY(180);
	model_view *= Translate(c_x, 0.0f, c_z);
	if (move)
		model_view *= Translate(0.0f, 0.0f, TIME * 2);
	else
		model_view *= Translate(0.0f, 0.0f, 24);
	set_colour(1.0f, 1.0f, 1.0f);
	glActiveTexture(GL_TEXTURE0);
	
	glBindTexture(GL_TEXTURE_2D, texture_flag);
	glUniform1i(uEnableTex, 1);

	//draw main body
	model_view *= Translate(0.0f, 2.0f, 0.0f);
	model_view *= Scale(6.0f, 2.0f, 10.0f);
	drawCube();
	
	//glUniform1i(uEnableTex, 0);
	mvstack.push(model_view);
	mvstack.push(model_view);
	mvstack.push(model_view);
	//draw turrent
	model_view *= Translate(0.0f, 0.5f, 0.0f);
	model_view *= Scale(0.3f, 0.5f, 0.3f);
	model_view *= RotateX(90);
	drawCylinder();
	model_view *= RotateX(-90);

	model_view *= Translate(0.0f, 1.5f, 0.0f);
	model_view *= Scale(2.0f, 1.8f, 2.0f);
	drawCube();

	//draw barrel
	model_view *= Translate(0.0f, 0.0f, 0.5f);
	model_view *= Translate(0.0f, 0.0f, 0.5f);
	model_view *= Scale(0.05f, 0.1f, 1.0f);
	drawCylinder();

	//draw flag
	model_view = mvstack.pop();
	model_view *= Translate(0.0f, 2.0f, 0.0f);
	model_view *= Scale(0.01f, 2.0f, 0.005f);
	model_view *= RotateX(90);
	drawCylinder();
	model_view = mvstack.pop();
	model_view *= Translate(0.0f, 3.8f, -0.05f);
	model_view *= Scale(0.02f, 0.5f, 0.1f);
	
	drawCube();
	glBindTexture(GL_TEXTURE_2D, texture_tank);
	//draw wheels
	model_view=mvstack.pop();
	model_view *= Scale(0.08f, 0.5f, 0.1f);
	mvstack.push(model_view);
	model_view *= Translate(6.0f, 0.0f, 0.0f);
	model_view *= RotateY(90);
	mvstack.push(model_view);
	mvstack.push(model_view);
	mvstack.push(model_view);
	mvstack.push(model_view);
	mvstack.push(model_view);
	for (int i = -4; i <= 4; i += 2){
		model_view = mvstack.pop();
		model_view *= Translate(i, 0.0f, 0.0f);
		if (spin)
			model_view *= RotateZ(90 * TIME);
		drawCylinder();
		model_view *= Translate(0.0f, 0.0f, 1.0f);
		model_view *= Scale(2.0f, 0.5f, 1.0f);
		drawCube();
		model_view *= Scale(0.25f, 4.0f, 1.0f);
		drawCube();
	}

	model_view = mvstack.pop();
	model_view *= Translate(-6.0f, 0.0f, 0.0f);
	model_view *= RotateY(90);
	mvstack.push(model_view);
	mvstack.push(model_view);
	mvstack.push(model_view);
	mvstack.push(model_view);
	mvstack.push(model_view);
	for (int i = -4; i <= 4; i += 2){
		model_view = mvstack.pop();
		model_view *= Translate(i, 0.0f, 0.0f);
		if (spin)
			model_view *= RotateZ(90 * TIME);
		drawCylinder();
		model_view *= Translate(0.0f, 0.0f, -1.0f);
		model_view *= Scale(2.0f, 0.5f, 1.0f);
		drawCube();
		model_view *= Scale(0.25f, 4.0f, 1.0f);
		drawCube();
	}
	glUniform1i(uEnableTex, 0);
}




/*********************************************************
	Class plane
**********************************************************/
class plane
{
public:
	plane(int x, int y, int z);
	void drawPlane();
	void startMove(){move = true;}
	void dropBomb1();
	void dropBomb2();

private:
	void drawBombs();
	int c_x;
	int c_y;
	int c_z;
	int temp;
	int lastTime;
	bool move;
	mat4 bomb1_position;
	int  bomb1_time;
	bool bomb1_droped;
	mat4 bomb2_position;
	bool bomb2_droped;
	int  bomb2_time;
};


plane::plane(int x, int y, int z){
	c_x = x;
	c_y = y;
	c_z = z;
	bomb1_droped = false;
	bomb2_droped = false;
	temp = 0;
	move = false;

}

void plane::drawBombs(){
	set_colour(1.0f, 0.0f, 0.0f);
	if (bomb1_droped){
		model_view = bomb1_position;
		model_view *= Translate(0.0f, -5 * (TIME - bomb1_time), 0.0f);
		drawSphere();
		
	}
	if (bomb2_droped){
		model_view = bomb2_position;
		model_view *= Translate(0.0f, -5 * (TIME - bomb2_time), 0.0f);
		drawSphere();
		
	}
}


void plane::dropBomb1(){
	if (!bomb1_droped)
		bomb1_droped = true;
}

void plane::dropBomb2(){
	if (!bomb2_droped)
		bomb2_droped = true;
}

void plane::drawPlane(){
	resetView();
	model_view *= Translate(c_x,c_y,c_z);
	

	if (move){

		model_view *= Translate(-(TIME - lastTime) * 40, 0, 0);
	}else{
		lastTime = TIME;
	}
	model_view *= RotateY(-90);
	set_colour(1.0f, 1.0f, 1.0f);
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, texture_plane);
	glUniform1i(uEnableTex, 1);

	// Draw main frame
	model_view *= Scale(3.0f, 3.0f, 40.0f);
	drawCube();
	model_view *= Scale(0.5f, 0.5f, 0.5f);

	// Draw head
	model_view *= Translate(0.0f, 0.0f, 1.2f);
	model_view *= Scale(1.0f, 1.0f, -0.2f);
	drawCone();
	

	//Draw propeller
	model_view *= Translate(0.0f, 0.0f, 1.1f);
	drawSphere();
	model_view *= Translate(0.0f, 0.0f, -1.1f);
	mvstack.push(model_view);
	model_view *= RotateZ(TIME * 1000);
	model_view *= Translate(0.0f, 0.0f, 0.3f);
	model_view *= Scale(1.3f, 1.3f, 1.0f);
	model_view *= Scale(5.0f, 0.5f, 0.2f);
	drawCube();
	model_view *= Scale(0.1f, 10.0f, 1.0f);
	drawCube();
	model_view = mvstack.pop();

	// Draw two main wings
	mvstack.push(model_view);
	mvstack.push(model_view);
	model_view *= Translate(6.0f, 0.0f, 3.0f);
	model_view *= Scale(12.0f, 0.5f, -3.0f);

	drawCube();
	model_view *= Translate(-1.0f, 0.0f, 0.0f);

	drawCube();
	model_view = mvstack.pop();

	//draw tail wings
	mvstack.push(model_view);
	model_view *= Translate(2.5f, 0.0f, 10.0f);
	model_view *= Scale(5.0f, 0.4f, 1.5f);
	drawCube();
	model_view *= Translate(-1.0f, 0.0f, 0.0f);
	drawCube();
	model_view = mvstack.pop();

	model_view *= Translate(0.0f, 2.5f, 10.5f);
	model_view *= Scale(0.3f, 3.0f, 1.0f);
	drawCube();
	glUniform1i(uEnableTex, 0);

	//draw bombs
	model_view = mvstack.pop();

	set_colour(1.0f, 0.0f, 0.0f);
	model_view *= Translate(6.0f, -1.0f, 3.0f);
	model_view *= Scale(1.0f, 1.0f, 2.0f);
	if (!bomb1_droped){
		bomb1_position = model_view;
		bomb1_time = TIME;
		drawSphere();
	}
	model_view *= Translate(-12.0f, 0.0f, 0.0f);
	if (!bomb2_droped){
		bomb2_position = model_view;
		bomb2_time = TIME;
		drawSphere();
	}

	drawBombs();

}

void camera(){
	//view houses
	if (TIME < 5.0){
		eye.x = -100  + TIME * 40;
		eye.y = 10;
		eye.z = -50;
		ref.x = 0;
		ref.y = 0;
		ref.z = 0;
	//look at zombie tanks
	} else if (TIME >= 5.0 && TIME < 8.0){
		eye.x = 100;
		eye.y = 0 + TIME * 2;
		eye.z = -50;
		ref.x = 0.0;
		ref.y = -10 + TIME * 2;
		ref.z = -500.0 + TIME * 100;
	}
	//look into them from back
	else if(TIME >=8.0 && TIME <12.0){
		eye.x = 0.0;
		eye.y = 26;
		eye.z = -50 + TIME * 20;
		ref.x = 0.0;
		ref.y = 26 - TIME;
		ref.z = 80;
	}
	// closer view at one tank
	else if(TIME >=12.0 && TIME <= 16.0){
		eye.x = 0 + 20 * sin(TIME - 16);
		eye.y = 16;
		eye.z = 76 + 20 * cos(TIME - 16);
		ref.x = 0.0;
		ref.y = 1.0;
		ref.z = 76.0;
	//view them shoot
	}else if(TIME >= 16.0 && TIME < 28.0){
		eye.x = -320 + 20 * TIME;
		eye.y = 16;
		eye.z = 92 - TIME;
		ref.x = 0.0;
		ref.y = 1.0;
		ref.z = 92 - TIME;
	//found planes
	}else if (TIME >= 28.0 && TIME < 34.0){
		eye.x = 240 ;
		eye.y = 16;
		eye.z = 64;
		ref.x = 50 * TIME - 1400;
		ref.y = TIME * 7 - 195;
		ref.z = TIME + 26;	
	}//look at them
	else if (TIME >= 34.0 && TIME <40.0){
		eye.x = 300 + 50 * sin(TIME);
		eye.y = 55;
		eye.z = 76 + 40 *cos(TIME);
		ref.x = 300;
		ref.y = 40;
		ref.z = 76;
	}
	//board view
	else{
		eye.x = 0;
		eye.y = 55;
		eye.z = 100 ;
		ref.x = 50;
		ref.y = 20;
		ref.z = 76;
	}



}
#define PLANE_NUM 5
#define TANKS_NUM 10

std::array<plane*, PLANE_NUM> planes;
std::array<tank*, TANKS_NUM> tanks;

void tanksShoot(){
	resetView();
	set_colour(1.0f, 0.0f, 0.0f);
	model_view *= Translate(5.0f, 6.0f, 66.0f - TIME * 5 + 76.0f );
	for (int i = 0; i < TANKS_NUM; i++)
		mvstack.push(model_view);
	for (int i = 0; i < TANKS_NUM; i++){
		model_view = mvstack.pop();
		model_view *= Translate(-200 + 40 * i, 0, 0);
		model_view *= Scale(0.2f, 0.2f, 0.2f);
		drawSphere();
	}
}
void display(void)
{

    // Clear the screen with the background colour (set in myinit)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	resetView();
	drawGround();
	drawSun();

	set_colour(1.0f, 1.0f, 1.0f);
	

	//houses
	if (TIME < 28){
		for (int i = -200; i < 200; i += 10){
			for (int j = -100; j < 0; j += 10){
				drawHouse(i, j);
			}
		}
	}else{
		for (int i = -200; i < 200; i += 10){
			for (int j = -100; j < -10; j += 10){
				drawHouse(i, j);
			}
		}
	}

	//tanks
	if (TIME == 0){
		for (int i = 0; i < TANKS_NUM; i++){
			tanks[i] = new tank(-200 + 40 * i, -100);
		}

		for (int i = 0; i < PLANE_NUM; i++){
			planes[i] = new plane( 300 + 50 * i, 40, 76);
		}
	}else if (TIME < 5){

	}else if (TIME > 5 && TIME <12.0){
		for (int i = 0; i < TANKS_NUM; i++){
			tanks[i]->drawtank(true, true);
		}
	}else if (TIME > 12.0 && TIME < 16.0){
		for (int i = 0; i < TANKS_NUM; i++){
			tanks[i]->drawtank(false, true);
		}
	}else if (TIME > 16.0 && TIME < 28.0){
		for (int i = 0; i < TANKS_NUM; i++){
			tanks[i]->drawtank(false, false);
		}
		tanksShoot();
	}else{
		for (int i = 0; i < TANKS_NUM; i++){
			tanks[i]->drawtank(false, false);
		}
	}


	//planes
	if (TIME >= 40){
		for (int i = 0; i < PLANE_NUM; i++){
			planes[i]->startMove();
		}
	}

	if (TIME >43){
		for (int i = 0; i < PLANE_NUM; i++){
			planes[i]->dropBomb1();
		}
	}

	if (TIME >47){
		for (int i = 0; i < 5; i++){
			tanks[i]->destory();
		}
	}


	if (TIME >48){
		for (int i = 0; i < PLANE_NUM; i++){
			planes[i]->dropBomb2();
		}
	}
	if (TIME >= 28){
		for (int i = 0; i < PLANE_NUM; i++){
			planes[i]->drawPlane();
		}

	}


	printFPS();
	camera();

	model_view = LookAt(eye, ref, up);
	glutSwapBuffers();
    if(Recording == 1)
        FrSaver.DumpPPM(Width, Height) ;
}

/**********************************************
    PROC: myReshape()
    DOES: handles the window being resized 
    
      -- don't change
**********************************************************/

void myReshape(int w, int h)
{
    Width = w;
    Height = h;

    glViewport(0, 0, w, h);

    mat4 projection = Perspective(50.0f, (float)w/(float)h, 1.0f, 1000.0f);
    glUniformMatrix4fv( uProjection, 1, GL_TRUE, projection );
}

void instructions() 
{
    printf("Press:\n");
    printf("  s to save the image\n");
    printf("  r to restore the original view.\n") ;
    printf("  0 to set it to the zero state.\n") ;
    printf("  a to toggle the animation.\n") ;
    printf("  m to toggle frame dumping.\n") ;
    printf("  q to quit.\n");
}

// start or end interaction
void myMouseCB(int button, int state, int x, int y)
{
    Button = button ;
    if( Button == GLUT_LEFT_BUTTON && state == GLUT_DOWN )
    {
        HVect arcball_coords;
        arcball_coords.x = 2.0*(float)x/(float)Width-1.0;
        arcball_coords.y = -2.0*(float)y/(float)Height+1.0;
        Ball_Mouse(Arcball, arcball_coords) ;
        Ball_Update(Arcball);
        Ball_BeginDrag(Arcball);

    }
    if( Button == GLUT_LEFT_BUTTON && state == GLUT_UP )
    {
        Ball_EndDrag(Arcball);
        Button = -1 ;
    }
    if( Button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN )
    {
        PrevY = y ;
    }


    // Tell the system to redraw the window
    glutPostRedisplay() ;
}

// interaction (mouse motion)
void myMotionCB(int x, int y)
{
    if( Button == GLUT_LEFT_BUTTON )
    {
        HVect arcball_coords;
        arcball_coords.x = 2.0*(float)x/(float)Width - 1.0 ;
        arcball_coords.y = -2.0*(float)y/(float)Height + 1.0 ;
        Ball_Mouse(Arcball,arcball_coords);
        Ball_Update(Arcball);
        glutPostRedisplay() ;
    }
    else if( Button == GLUT_RIGHT_BUTTON )
    {
        if( y - PrevY > 0 )
            Zoom  = Zoom * 1.03 ;
        else 
            Zoom  = Zoom * 0.97 ;
        PrevY = y ;
        glutPostRedisplay() ;
    }
}


void idleCB(void)
{
    if( Animate == 1 )
    {
        // TM.Reset() ; // commenting out this will make the time run from 0
        // leaving 'Time' counts the time interval between successive calls to idleCB
        if( Recording == 0 )
            TIME = TM.GetElapsedTime() ;
        else
            TIME += 0.033 ; // save at 30 frames per second.
        
        
        printf("TIME %f\n", TIME) ;
        glutPostRedisplay() ; 
    }
}
/*********************************************************
     PROC: main()
     DOES: calls initialization, then hands over control
           to the event handler, which calls 
           display() whenever the screen needs to be redrawn
**********************************************************/

int main(int argc, char** argv) 
{
    glutInit(&argc, argv);
    // If your code fails to run, uncommenting these lines may help.
    //glutInitContextVersion(3, 2);
    //glutInitContextProfile(GLUT_CORE_PROFILE);
    glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowPosition (0, 0);
    glutInitWindowSize(Width,Height);
    glutCreateWindow("Xiaohui: Fight against zombie tanks");
    printf("GL version %s\n", glGetString(GL_VERSION));
    glewExperimental = GL_TRUE;
    glewInit();
    
    myinit();

    glutIdleFunc(idleCB) ;
    glutReshapeFunc (myReshape);
    glutKeyboardFunc( myKey );
    glutMouseFunc(myMouseCB) ;
    glutMotionFunc(myMotionCB) ;
    instructions();

    glutDisplayFunc(display);
    glutMainLoop();

    TM.Reset() ;
    return 0;         // never reached
}




