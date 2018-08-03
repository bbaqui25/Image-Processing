//
// F#-based PPM image library.
//
// Prof. Joe Hummel
// U. of Illinois, Chicago
// CS341, Spring 2017
// Project 04: Solution
//

module PPMImageLibrary

#light


//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage (image:(int*int*int) list list) = 
  match image with
  | [] -> printfn "**END**"
  | hd::tl -> printfn "%A" hd
              OutputImage tl
           
let DebugOutput(width:int, height:int, depth:int, image:(int*int*int) list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage image


//
// TransformFirstThreeRows:
//
// An example transformation: replaces the first 3 rows of the given image
// with a row of Red, White and Blue pixels (go USA :-).
//
let rec BuildRowOfThisColor row color = 
  match row with
  | []     -> []
  | hd::tl -> color :: BuildRowOfThisColor tl color

let TransformFirstThreeRows(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let row1 = List.head image
  let row2 = List.head (List.tail image)
  let row3 = List.head (List.tail (List.tail image))
  let tail = List.tail (List.tail (List.tail image))
  let newRow1 = BuildRowOfThisColor row1 (255,0,0)      // red:
  let newRow2 = BuildRowOfThisColor row2 (255,255,255)  // white:
  let newRow3 = BuildRowOfThisColor row3 (0,0,255)      // blue:
  let newImage = newRow1 :: newRow2 :: newRow3 :: tail
  newImage


//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let Flatten (SL:string list) = 
  List.reduce (fun s1 s2 -> s1 + " " + s2) SL

let Image2ListOfStrings (image:(int*int*int) list list) = 
  List.map (fun TL -> List.map (fun (r,g,b) -> r.ToString()+" "+g.ToString()+" "+b.ToString()+" ") TL) image
  |> List.map Flatten

let rec WriteP3Image(filepath:string, width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let L = [ "P3" ] @ 
          [ System.Convert.ToString(width); System.Convert.ToString(height) ] @
          [ System.Convert.ToString(depth) ] @
          (Image2ListOfStrings image)
  System.IO.File.WriteAllLines(filepath, L)
  true  // success



//
// Grayscale:
//
// Converts the image into grayscale and returns the resulting image as a list of lists. 
// Conversion to grayscale is done by averaging the RGB values for a pixel, and then 
// replacing them all by that average. So if the RGB values were 25 75 250, the average 
// would be 116, and then all three RGB values would become 116 — i.e. 116 116 116.
//
let private Pixel2Gray pixel = 
  let (R,G,B) = pixel
  let avg = (R+G+B) / 3
  (avg,avg,avg)

let Grayscale(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  List.map (fun row -> List.map Pixel2Gray row) image



//
// Threshold
//
// Thresholding increases image separation --- dark values become darker and light values
// become lighter.  Given a threshold value in the range 0 < threshold < MaxColorDepth,
// all RGB values > threshold become the max color depth (white) while all RGB values
// <= threshold become 0 (black).  The resulting image is returned.
//
let rec private Row2Threshold row depth threshold = 
  match row with
  | [ ]         -> []
  | pixel::tail -> let (R,G,B) = pixel
                   let newR = if R > threshold then depth else R
                   let newG = if G > threshold then depth else G
                   let newB = if B > threshold then depth else B
                   (newR,newG,newB)::(Row2Threshold tail depth threshold)

let rec Threshold(width:int, height:int, depth:int, image:(int*int*int) list list, threshold:int) = 
  List.map (fun row -> Row2Threshold row depth threshold) image



//
// FlipHorizontal:
//
// Flips an image so that what’s on the left is now on the right, and what’s on 
// the right is now on the left. That is, the pixel that is on the far left end
// of the row ends up on the far right of the row, and the pixel on the far right 
// ends up on the far left. This is repeated as you move inwards toward the center 
// of the row.
//
let private FlipRow row = 
  List.rev row

let rec FlipHorizontal(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  List.map (fun row -> FlipRow row) image



//
// Zoom:
//
// Zooms the image by the given zoom factor, which is an integer 0 < factor < 5.  
// The function uses the nearest neighbor approach where each pixel P in the original 
// image is replaced by a factor*factor block of P pixels.  For example, if the zoom 
// factor is 4, then each pixel is replaced by a 4x4 block of 16 identical pixels. 
// The nearest neighbor algorithm is the simplest zoom algorithm, but results in 
// jagged images.  The resulting image is returned.
//
let rec private zoomPixel pixel factor = 
  //match factor with
  //| 0 -> []
  //| _ -> pixel :: zoomPixel pixel (factor-1)
  let L = [1..factor]
  List.map (fun i -> pixel) L

let rec private zoomOneRow row factor = 
//  match row with
//  | [] -> []
//  | pixel::tail -> (zoomPixel pixel factor) @ (zoomOneRow tail factor)
  let LofL = List.map (fun pixel -> zoomPixel pixel factor) row
  let R = List.reduce (fun acc pixels -> pixels @ acc) LofL
  List.rev R

let private zoomRow row factor = 
  let L = [1..factor]
  List.map (fun i -> zoomOneRow row factor) L

let rec Zoom(width:int, height:int, depth:int, image:(int*int*int) list list, factor:int) = 
  let LofL = List.map (fun row -> zoomRow row factor) image
  List.reduce (fun acc L -> acc @ L) LofL



//
// RotateRight90:
//
// Rotates the image to the right 90 degrees.
//

//
// rotates one row of the image:  [(1,2,3); (4;5;6); (7;8;9)] becomes
// [ [(1;2;3)] ; [(4;5;6)] ; [(7;8;9)] ] --- notice the list of lists,
// so the result is now a column.
//
let rec _RotateOneRow L acc = 
  match L with 
  | []          -> List.rev acc
  | pixel::tail -> _RotateOneRow tail ([pixel] :: acc)

let rec RotateOneRow L = 
  _RotateOneRow L []

//
// merges two image slices into a single image --- this amounts to 
// concatenating the lists of pixels into one list.  We repeat this
// for all rows in the image.
//
let Merge image1 image2 =
  List.map2 (fun L1 L2 -> L1 @ L2) image1 image2

//
// rotates the image by rotating a row, rotating the rest 
// recursively, and then merging the results.  
//
let rec RotatePixels L = 
  match L with
  | []        -> []
  | row::[]   -> RotateOneRow row
  | row::tail -> Merge (RotatePixels tail) (RotateOneRow row)

let rec RotateRight90(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  RotatePixels image
