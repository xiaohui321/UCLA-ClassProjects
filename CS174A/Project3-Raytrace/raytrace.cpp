#include "matm.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cstdlib>

using namespace std;

#define _CRT_SECURE_NO_WARNINGS
#define MAX_SPHERE_OR_LIGHT_SOURCE_NUM 5

int g_width;
int g_height;

float g_left;
float g_right;
float g_top;
float g_bottom;
float g_near;

vector<vec4> g_colors;

vec4 background_color;
vec4 ambient_intensity;

string output_file_name;

struct Ray{
    vec4 origin;
    vec4 dir;
};

/**************************************
 * Sphere
 */
typedef struct{
    string name;
    //position    
    float position_x;
    float position_y;
    float position_z;
    //scaling
    float scale_x;
    float scale_y;
    float scale_z;
    //color in range 0 - 1
    float r;
    float g;
    float b;

    float k_a;
    float k_d;
    float k_s;
    float k_r;
    //specular exponent
    float n;
}Sphere;

int num_sphere;
Sphere spheres[MAX_SPHERE_OR_LIGHT_SOURCE_NUM];

/**************************************
 *  Light
 */
typedef struct{
    string name;
    //position    
    float position_x;
    float position_y;
    float position_z;
    //intensity
    float I_r;
    float I_g;
    float I_b;
}Light;

int num_light;
Light lights[MAX_SPHERE_OR_LIGHT_SOURCE_NUM];

/**************************************
 * Global variable initialization
 */
void init(){
    num_sphere = 0;
    num_light  = 0;
}

// ----------------------------------------------------------------------------
// Input file parsing

vec4 toVec4(const string& s1, const string& s2, const string& s3){
    stringstream ss(s1 + " " + s2 + " " + s3);
    vec4 result;
    ss >> result.x >> result.y >> result.z;
    result.w = 1.0f;
    return result;
}

float toFloat(const string& s){
    stringstream ss(s);
    float f;
    ss >> f;
    return f;
}

void parseLine(const vector<string>& vs){
    //parsing of NEAR,LEFT,RIGHT,BOTTOM,TOP,SPHERE,LIGHT,BACK,AMBIENT,OUTPUT.
    if      (vs[0] == "NEAR"){
        g_near   = toFloat(vs[1]);
    }else if(vs[0] == "LEFT"){
        g_left   = toFloat(vs[1]);

    }else if(vs[0] == "RIGHT"){
        g_right  = toFloat(vs[1]);

    }else if(vs[0] == "BOTTOM"){
        g_bottom = toFloat(vs[1]);

    }else if(vs[0] == "TOP"){
        g_top    = toFloat(vs[1]);

    }else if(vs[0] == "RES"){
        g_width  = (int)toFloat(vs[1]);
        g_height = (int)toFloat(vs[2]);
        g_colors.resize(g_width * g_height);

    }else if(vs[0] == "SPHERE"){
        if(num_sphere >= MAX_SPHERE_OR_LIGHT_SOURCE_NUM){
            cout << "too many spheres!\n";
            exit(1);
        }
        spheres[num_sphere].position_x = toFloat(vs[2]);
        spheres[num_sphere].position_y = toFloat(vs[3]);
        spheres[num_sphere].position_z = toFloat(vs[4]);
        spheres[num_sphere].scale_x    = toFloat(vs[5]);
        spheres[num_sphere].scale_y    = toFloat(vs[6]);
        spheres[num_sphere].scale_z    = toFloat(vs[7]);
        spheres[num_sphere].r          = toFloat(vs[8]);
        spheres[num_sphere].g          = toFloat(vs[9]);
        spheres[num_sphere].b          = toFloat(vs[10]);
        spheres[num_sphere].k_a        = toFloat(vs[11]);
        spheres[num_sphere].k_d        = toFloat(vs[12]);
        spheres[num_sphere].k_s        = toFloat(vs[13]);
        spheres[num_sphere].k_r        = toFloat(vs[14]);
        spheres[num_sphere].n          = toFloat(vs[15]);

        num_sphere++;

    }else if(vs[0] == "LIGHT"){
        if(num_light >= MAX_SPHERE_OR_LIGHT_SOURCE_NUM){
            cout << "too many lights!\n";
            exit(1);
        }
        lights[num_light].name       = vs[1];
        lights[num_light].position_x = toFloat(vs[2]);
        lights[num_light].position_y = toFloat(vs[3]);
        lights[num_light].position_z = toFloat(vs[4]);
        lights[num_light].I_r        = toFloat(vs[5]);
        lights[num_light].I_g        = toFloat(vs[6]);
        lights[num_light].I_b        = toFloat(vs[7]);

        num_light++;

    }else if(vs[0] == "BACK"){
        background_color  = toVec4(vs[1], vs[2], vs[3]);

    }else if(vs[0] == "AMBIENT"){
        ambient_intensity = toVec4(vs[1], vs[2], vs[3]);

    }else if(vs[0] == "OUTPUT"){
        output_file_name  = vs[1];
    }
    //otherwise empty line: ignore
}

void loadFile(const char* filename){
    ifstream is(filename);
    if (is.fail()){
        cout << "Could not open file " << filename << endl;
        exit(1);
    }
    string s;
    vector<string> vs;
    while(!is.eof()){
        vs.clear();
        getline(is, s);
        istringstream iss(s);
        while (!iss.eof()){
            string sub;
            iss >> sub;
            vs.push_back(sub);
        }
        parseLine(vs);
    }
}

// ----------------------------------------------------------------------------
// Utilities

void setColor(int ix, int iy, const vec4& color){
    int iy2 = g_height - iy - 1; // Invert iy coordinate.
    g_colors[iy2 * g_width + ix] = color;
}

// ----------------------------------------------------------------------------
// Intersection routine

// TODO: add your ray-sphere intersection routine here.
vec4 findIntersection(int& num, vec4 rayOrigin, vec4 rayDir, bool b_sphere){
    
    float a,b,c,d;
    float p, m, min;
    min = -1.0f;
    num = 0;

    for(int i = 0; i < num_sphere; i++){
        mat4 matrix_sphere(
            spheres[i].scale_x, 0.0f, 0.0f, 0.0f,
            0.0f, spheres[i].scale_y, 0.0f, 0.0f,
            0.0f, 0.0f, spheres[i].scale_z, 0.0f,
            spheres[i].position_x, spheres[i].position_y, spheres[i].position_z, 1.0f);
        
        mat4 matrix_sphere_Inv;

        InvertMatrix(matrix_sphere, matrix_sphere_Inv);

        vec4 point;
        point = matrix_sphere_Inv * rayOrigin;
        point.w = 0.0f;

        vec4 direction = matrix_sphere_Inv * rayDir;
        direction.w = 0.0f;

        a = dot(direction, direction);
        b = 2 * dot(point, direction);
        c = dot(point, point) - 1;
        d = b * b - 4 * a * c;

        if(d > 0){
            //more than one intersection
            p = ( sqrt(d) - b) / (2 * a);
            m = (-sqrt(d) - b) / (2 * a);
            if(p < m){
                if((min == -1 || p < min ) && p > 0){
                    num = 1 + i;
                    min = p;
                }
            }else{
               if((min == -1 || m < min ) && m > 0){
                    num = 1 + i;
                    min = m;
                } 
            }
        }else if (d == 0){//one intersection
            p = - b /(a * 2);
            if((min == -1 || p < min ) && p > 0){
                num = 1 + i;
                min = p;
            }
        }else{//no intersection
            continue;
        }
    }

    if(num != 0 || b_sphere){
        vec4 ret(rayOrigin.operator+(rayDir.operator*(min)));
        return ret;
    }else{
        vec4 ret(0.0f, 0.0f, 0.0f, 0.0f);
        return ret;
    }
}

vec4 getNormal(vec4 v, int sphere_num){
    int num = sphere_num -1;
    mat4 matrix_sphere(
            spheres[num].scale_x, 0.0f, 0.0f, 0.0f,
            0.0f, spheres[num].scale_y, 0.0f, 0.0f,
            0.0f, 0.0f, spheres[num].scale_z, 0.0f,
            spheres[num].position_x, spheres[num].position_y, spheres[num].position_z, 1.0f);
        
    mat4 matrix_sphere_Inv, matrix_sphere_Inv_Trans;
    InvertMatrix(matrix_sphere, matrix_sphere_Inv);
    matrix_sphere_Inv_Trans = transpose(matrix_sphere_Inv);
    v = matrix_sphere_Inv * v;
    vec4 ret = matrix_sphere_Inv_Trans * normalize(v);
    ret.w = 0.0f;
    return normalize(ret);
}

vec4 getReflection(vec4 light, vec4 normal){
    vec4 ret((normal.operator*(2 * dot(normal,light))).operator-(light));
    return normalize(ret);
}

vec4 getDiffusion(vec4 intersection_point, vec4 n, vec4 reflect, int sphere_num){
    int num = sphere_num -1;
    float r = 0.0f;
    float g = 0.0f;
    float b = 0.0f;
    for(int i = 0; i < num_light; i++){
        vec4 light(lights[i].position_x, lights[i].position_y, lights[i].position_z, 1.0f);
        light.operator-=(intersection_point);
        light.w = 0;
        light = normalize(light);
        
        //ignore this light if > 90 degree
        if(dot(light, n) < 0)
            continue;
        else{
            int shadow_num;
            findIntersection(shadow_num, intersection_point, light, true);

            if(shadow_num == 0){
                r += lights[i].I_r * spheres[num].k_d * dot(n, light) * spheres[num].r;
                g += lights[i].I_g * spheres[num].k_d * dot(n, light) * spheres[num].g;
                b += lights[i].I_b * spheres[num].k_d * dot(n, light) * spheres[num].b;
            }
        }
    }
    vec4 colors(r, g, b, 0.0f);
    return colors;
}

vec4 getSpeculation(vec4 intersection_point, vec4 n, int sphere_num){
    int num = sphere_num -1;
    float r = 0.0f;
    float g = 0.0f;
    float b = 0.0f;
    vec4 v_intersect = normalize(intersection_point);
    for(int i = 0; i < num_light; i++){
        vec4 light(lights[i].position_x, lights[i].position_y, lights[i].position_z, 1.0f);
        light.operator-=(intersection_point);
        light.w = 0;
        light = normalize(light);
        
        //ignore this light if > 90 degree
        if(dot(light, n) < 0)
            continue;
        else{
            int shadow_num;
            findIntersection(shadow_num, intersection_point, light, true);

            if(shadow_num == 0){
                vec4 reflect = getReflection(light, n);
                r += lights[i].I_r * spheres[num].k_s * pow(dot(reflect, v_intersect), spheres[num].n);
                g += lights[i].I_g * spheres[num].k_s * pow(dot(reflect, v_intersect), spheres[num].n);
                b += lights[i].I_b * spheres[num].k_s * pow(dot(reflect, v_intersect), spheres[num].n);
            }
        }
    }
    vec4 colors(r,g,b,0.0f);
    return colors;
}

// ----------------------------------------------------------------------------
// Ray tracing

vec4 trace(const Ray& ray, int level){
    if(level > 2)
        return (0.0f, 0.0f, 0.0f, 0.0f);

    int num = 0;
    vec4 intersection_point = findIntersection(num, ray.origin, ray.dir, false);

    if(num == 0){
        if (level > 1)
            return (0.0f, 0.0f, 0.0f, 0.0f);
        else
            return background_color;
    }else if(num < 10){
        vec4 colors(
            spheres[num-1].r,
            spheres[num-1].g,
            spheres[num-1].b, 1.0f);

        colors.operator*=(ambient_intensity.operator*(spheres[num - 1].k_a));

        vec4 reflect;
        vec4 normal_vector = getNormal(intersection_point, num);
        for(int i = 0; i < num_light; i++){
            vec4 light(lights[i].position_x, lights[i].position_y, lights[i].position_z, 1.0f);
            light.operator-=(intersection_point);
            light.w = 0;
            light = normalize(light);

            //ignore this light if > 90 degree
            if(dot(light, normal_vector) < 0)
                continue;
            else{
                reflect = getReflection(light, normal_vector);
                Ray ref;
                ref.origin = intersection_point;
                ref.dir    = reflect;
                vec4 reflected_color = trace(ref, level + 1);
                colors.operator+=(reflected_color.operator*(spheres[num - 1].k_r));
            }
        }
        vec4 diffuse_color = getDiffusion(intersection_point, normal_vector, reflect, num);
        colors.operator+=(diffuse_color);

        vec4 specular_color =getSpeculation(intersection_point,normal_vector, num);
        colors.operator+=(specular_color);

        colors.w = 0.0f;
        return colors;

    }else{
        vec4 ret(
            lights[num - 10].I_r, 
            lights[num - 10].I_g, 
            lights[num - 10].I_b, 1.0f);
        return ret;
    }
}

vec4 getDir(int ix, int iy){
    //return the direction from the origin to pixel (ix, iy), normalized.
    vec4 dir;
    float x = ((float)ix) * (g_right - g_left)/((float)g_width) + g_left;
    float y = ((float)iy) * (g_top - g_bottom)/((float)g_height) + g_bottom;
    dir = vec4(x, y, -g_near, 0.0f);
    dir = normalize(dir);
    return dir;
}

void renderPixel(int ix, int iy){
    Ray ray;
    ray.origin = vec4(0.0f, 0.0f, 0.0f, 1.0f);
    ray.dir = getDir(ix, iy);
    vec4 color = trace(ray,1);
    setColor(ix, iy, color);
}

void render(){
    for (int iy = 0; iy < g_height; iy++)
        for (int ix = 0; ix < g_width; ix++)
            renderPixel(ix, iy);
}

// ----------------------------------------------------------------------------
// PPM saving

void savePPM(int Width, int Height, char* fname, unsigned char* pixels){
    FILE *fp;
    const int maxVal=255;

    printf("Saving image %s: %d x %d\n", fname, Width, Height);
    fp = fopen(fname,"wb");
    if (!fp) {
        printf("Unable to open file '%s'\n", fname);
        return;
    }
    fprintf(fp, "P6\n");
    fprintf(fp, "%d %d\n", Width, Height);
    fprintf(fp, "%d\n", maxVal);

    for(int j = 0; j < Height; j++) {
        fwrite(&pixels[j*Width*3], 3, Width, fp);
    }

    fclose(fp);
}

void saveFile(){
    // Convert color components from floats to unsigned chars.
    // clamp values if out of range.
    unsigned char* buf = new unsigned char[g_width * g_height * 3];
    for (int y = 0; y < g_height; y++)
        for (int x = 0; x < g_width; x++)
            for (int i = 0; i < 3; i++){
                float value = ((float*) g_colors[y * g_width + x])[i];
                if(value > 1.0)
                    value = 1.0; 
                buf[y*g_width*3+x*3+i] = (unsigned char)(value * 255.9f);
            }

    //file name based on input file name.
    savePPM(g_width, g_height, &output_file_name[0], buf);
    delete[] buf;
}

// ----------------------------------------------------------------------------
// Main

int main(int argc, char* argv[]){
    if (argc < 2){
        cout << "Usage: template-rt <input_file.txt>" << endl;
        exit(1);
    }
    init();
    loadFile(argv[1]);
    render();
    saveFile();
	return 0;
}
