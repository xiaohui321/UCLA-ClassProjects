import java.io.*;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveTask;
import java.util.concurrent.RecursiveAction;

// a marker for code that you need to implement
class ImplementMe extends RuntimeException {}

// an RGB triple
class RGB {
	public int R, G, B;

	RGB(int r, int g, int b) {
		R = r;
		G = g;
		B = b;
	}

	public String toString() { return "(" + R + "," + G + "," + B + ")"; }

}

// code for creating a Gaussian filter
class Gaussian {

	protected static double gaussian(int x, int mu, double sigma) {
		return Math.exp( -(Math.pow((x-mu)/sigma,2.0))/2.0 );
	}

	public static double[][] gaussianFilter(int radius, double sigma) {
		int length = 2 * radius + 1;
		double[] hkernel = new double[length];
		for(int i=0; i < length; i++)
			hkernel[i] = gaussian(i, radius, sigma);
		double[][] kernel2d = new double[length][length];
		double kernelsum = 0.0;
		for(int i=0; i < length; i++) {
			for(int j=0; j < length; j++) {
				double elem = hkernel[i] * hkernel[j];
				kernelsum += elem;
				kernel2d[i][j] = elem;
			}
		}
		for(int i=0; i < length; i++) {
			for(int j=0; j < length; j++)
				kernel2d[i][j] /= kernelsum;
		}
		return kernel2d;
	}
}

class NegateTask extends RecursiveAction{

	protected int maxColorVal, start, end;

	protected RGB[] original_pixels, new_pixels;

	protected static final int SEQUENTIAL_THRESHOLD = 10000;

	NegateTask(RGB[] new_pixels, RGB[] original_pixels, int start, int end, int maxColorVal){
		this.maxColorVal     = maxColorVal;
		this.start           = start;
		this.end             = end;
		this.original_pixels = original_pixels;
		this.new_pixels      = new_pixels;
	}

	public void compute(){
		if(end - start < SEQUENTIAL_THRESHOLD){
			for(int i = start; i< end; i++){
				new_pixels[i] = new RGB(
					maxColorVal - original_pixels[i].R,
					maxColorVal - original_pixels[i].G,
					maxColorVal - original_pixels[i].B);
			}
		}else{
			int mid = (start + end) / 2;
			NegateTask first  = new NegateTask(new_pixels, original_pixels, start, mid, maxColorVal);
			NegateTask second = new NegateTask(new_pixels, original_pixels, mid, end, maxColorVal);
			first.fork();
			second.compute();
			first.join();
			return;
		}

	}
}


class MirrorTask extends RecursiveAction{
	
	protected int start, end, width;

	protected RGB[] original_pixels, new_pixels;

	// number of lines per thread
	protected static final int SEQUENTIAL_THRESHOLD = 100;

	MirrorTask(RGB[] new_pixels, RGB[] original_pixels, int start, int end, int width){
		this.width           = width;
		this.start           = start;
		this.end             = end;
		this.original_pixels = original_pixels;
		this.new_pixels      = new_pixels;
	}

	public void compute(){
		if(end - start < SEQUENTIAL_THRESHOLD){
			for(int i = start; i< end; i++){
				int beg =  i *  width;
				int end = (i+1) * width - 1;
				for(int j = 0; j< width; j++){
					new_pixels[beg + j] = new RGB(
						original_pixels[end - j].R, 
						original_pixels[end - j].G, 
						original_pixels[end - j].B);
				}
			}
		}else{
			int mid = (start + end) / 2;
			MirrorTask first = new MirrorTask(new_pixels, original_pixels, start, mid, width);
			MirrorTask second = new MirrorTask(new_pixels, original_pixels, mid, end, width);
			first.fork();
			second.compute();
			first.join();
			return;
		}

	}
}

class BlurTask extends RecursiveAction{

	protected int start, end, width, height, radius;

	protected double[][] filter;

	protected RGB[] original_pixels, new_pixels;

	protected static final int SEQUENTIAL_THRESHOLD = 100;

	BlurTask(RGB[] new_pixels, RGB[] original_pixels, int start, int end, int width, int height, int radius, double [][] filter){
		this.width           = width;
		this.height          = height;
		this.radius          = radius;
		this.start           = start;
		this.end             = end;
		this.original_pixels = original_pixels;
		this.new_pixels      = new_pixels;
		this.filter          = filter;
	}

	public void compute(){
		
		if(end - start < SEQUENTIAL_THRESHOLD){
			int r,g,b,x,y;
			double filterValue;
			for(int i = start; i< end; i++){
				for(int j = 0; j< width; j++){
					
					r = 0;
					g = 0;
					b = 0;
					for(int k = i - radius; k <= i + radius ; k++){
						for(int l = j - radius; l <= j + radius ; l++){

							//check if out of bounds
							if(k<0)
								x = 0;
							else if (k >=height)
								x = height -1;
							else
								x = k;

							if(l<0)
								y = 0;
							else if (l >=width)
								y = width -1;
							else
								y = l;

							filterValue = filter[k + radius - i][l + radius - j];
							r +=original_pixels[x*width + y].R * filterValue;
							g +=original_pixels[x*width + y].G * filterValue;
							b +=original_pixels[x*width + y].B * filterValue;
						}
					}
					new_pixels[i * width + j] = new RGB(r,g,b);
				}
			}
		}else{
			int mid = (start + end) / 2;
			BlurTask first  = new BlurTask(new_pixels, original_pixels, start, mid, width, height, radius, filter);
			BlurTask second = new BlurTask(new_pixels, original_pixels, mid, end, width, height, radius, filter);
			first.fork();
			second.compute();
			first.join();
			return;
		}

	}
}


// an object representing a single PPM image
class PPMImage {
	protected int width, height, maxColorVal;
	protected RGB[] pixels;

	PPMImage(int w, int h, int m, RGB[] p) {
		width = w;
		height = h;
		maxColorVal = m;
		pixels = p;
	}

	// parse a PPM file to produce a PPMImage
	public static PPMImage fromFile(String fname) throws FileNotFoundException, IOException {
		FileInputStream is = new FileInputStream(fname);
		BufferedReader br = new BufferedReader(new InputStreamReader(is));
		br.readLine(); // read the P6
		String[] dims = br.readLine().split(" "); // read width and height
		int width = Integer.parseInt(dims[0]);
		int height = Integer.parseInt(dims[1]);
		int max = Integer.parseInt(br.readLine()); // read max color value
		br.close();

		is = new FileInputStream(fname);
	    // skip the first three lines
		int newlines = 0;
		while (newlines < 3) {
			int b = is.read();
			if (b == 10)
				newlines++;
		}

		int MASK = 0xff;
		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
		is.read(bytes);
		RGB[] pixels = new RGB[numpixels];
		for (int i = 0; i < numpixels; i++) {
			int offset = i * 3;
			pixels[i] = new RGB(bytes[offset] & MASK, bytes[offset+1] & MASK, bytes[offset+2] & MASK);
		}

		return new PPMImage(width, height, max, pixels);
	}

	// write a PPMImage object to a file
	public void toFile(String fname) throws IOException {
		FileOutputStream os = new FileOutputStream(fname);

		String header = "P6\n" + width + " " + height + "\n" + maxColorVal + "\n";
		os.write(header.getBytes());

		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
		int i = 0;
		for (RGB rgb : pixels) {
			bytes[i] = (byte) rgb.R;
			bytes[i+1] = (byte) rgb.G;
			bytes[i+2] = (byte) rgb.B;
			i += 3;
		}
		os.write(bytes);
	}

	public PPMImage negate() {
		int size = width * height; 
		RGB[] new_pixels = new RGB[size];

		/*
		//sequential		
		for(int i = 0; i< size; i++){
			new_pixels[i] = new RGB(
				maxColorVal - pixels[i].R,
				maxColorVal - pixels[i].G,
				maxColorVal - pixels[i].B);
		}
		*/

		//parallel
		NegateTask t = new NegateTask(new_pixels, pixels, 0, size, maxColorVal);
		ForkJoinPool pool = new ForkJoinPool();
		pool.invoke(t);
		return new PPMImage(width,height,maxColorVal,new_pixels);
	}

	public PPMImage mirrorImage() {
		int size = width * height; 
		RGB[] new_pixels = new RGB[size];

		/*
		//sequential
		for(int i = 0; i< height; i++){
			int beg =  i *  width;
			int end = (i+1) * width - 1;
			for(int j = 0; j< width; j++){
				new_pixels[beg + j] = new RGB(
					pixels[end - j].R, 
					pixels[end - j].G, 
					pixels[end - j].B);

			}
		}
		*/

		//parallel
		MirrorTask t = new MirrorTask(new_pixels, pixels, 0, height, width);
		ForkJoinPool pool = new ForkJoinPool();
		pool.invoke(t);

		return new PPMImage(width,height,maxColorVal,new_pixels);
	}

	public PPMImage gaussianBlur(int radius, double sigma) {
		double[][] filter = Gaussian.gaussianFilter(radius,sigma);

		int size = width * height; 
		RGB[] new_pixels = new RGB[size];

		/*
		//sequential
		int r,g,b,x,y;
		double filterValue;
		for(int i = 0; i< height; i++){
			for(int j = 0; j< width; j++){
				
				r = 0;
				g = 0;
				b = 0;
				for(int k = i - radius; k <= i + radius ; k++){
					for(int l = j - radius; l <= j + radius ; l++){

						//check if out of bounds
						if(k<0)
							x = 0;
						else if (k >=height)
							x = height -1;
						else
							x = k;

						if(l<0)
							y = 0;
						else if (l >=width)
							y = width -1;
						else
							y = l;

						filterValue = filter[k + radius - i][l + radius - j];
						r +=pixels[x*width + y].R * filterValue;
						g +=pixels[x*width + y].G * filterValue;
						b +=pixels[x*width + y].B * filterValue;
					}
				}
				new_pixels[i * width + j] = new RGB(r,g,b);
			}
		}
		*/

		//parallel
		BlurTask t = new BlurTask(new_pixels, pixels, 0, height, width, height, radius, filter);
		ForkJoinPool pool = new ForkJoinPool();
		pool.invoke(t);

		return new PPMImage(width,height,maxColorVal,new_pixels);
	}
		
}

class TestMain{
	public static void main(String[] args){
		try{
			PPMImage original_image = PPMImage.fromFile("florence.ppm");
			PPMImage negated_image  = original_image.negate();
			PPMImage mirrored_image = original_image.mirrorImage();
			PPMImage blured_image   = original_image.gaussianBlur(5,2.0);

			negated_image.toFile("negate.ppm");
			mirrored_image.toFile("mirrored.ppm");
			blured_image.toFile("blured.ppm");
		
		}catch (FileNotFoundException e){
			System.out.println("Can not find file : florence" );
		}catch (IOException e){
			System.out.println("Can not open/write files" );
		}
	}
}