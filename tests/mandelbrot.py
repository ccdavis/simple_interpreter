def mandelbrot(h, w, max_iter):
    """
    Returns a list of lists representing the Mandelbrot set.
    h: height of the output image
    w: width of the output image
    max_iter: maximum number of iterations to check for convergence
    """
    # Define the region of the complex plane to plot
    y_min, y_max = -1.2, 1.2
    x_min, x_max = -2.0, 1.0
    
    # Initialize the output array
    result = [[0 for _ in range(w)] for _ in range(h)]
    
    # For each pixel in the output image
    for i in range(h):
        for j in range(w):
            # Convert pixel coordinates to complex plane coordinates
            x = x_min + j * (x_max - x_min) / (w - 1)
            y = y_min + i * (y_max - y_min) / (h - 1)
            
            # The complex number to test
            c = complex(x, y)
            z = complex(0, 0)
            
            # Iterate to see if the point escapes
            for k in range(max_iter):
                z = z * z + c
                
                # If the point escapes
                if abs(z) > 2:
                    # Color is based on how quickly the point escapes
                    result[i][j] = k
                    break
            
            # If the point doesn't escape, it's in the Mandelbrot set
            # We leave it as 0 in the result
    
    return result

def print_mandelbrot(mandelbrot_set):
    """
    Print a simple ASCII representation of the Mandelbrot set.
    """
    h = len(mandelbrot_set)
    w = len(mandelbrot_set[0])
    
    # Define a set of characters with increasing "density"
    chars = ' .:-=+*#%@'
    max_value = max(max(row) for row in mandelbrot_set)
    
    for i in range(h):
        for j in range(w):
            # Scale the value to the range of characters
            if mandelbrot_set[i][j] == 0:
                # Points in the set
                print('@', end='')
            else:
                # Points outside the set, with a character representing how quickly they escape
                index = (len(chars) - 1) * mandelbrot_set[i][j] // max_value
                print(chars[index], end='')
        print()  # New line after each row

# Set parameters
height = 100
width = 120
max_iterations = 1000

# Generate and print the Mandelbrot set
mandelbrot_set = mandelbrot(height, width, max_iterations)
print_mandelbrot(mandelbrot_set)

# For a better view, let's also save the output to a file
with open('mandelbrot.txt', 'w') as f:
    chars = ' .:-=+*#%@'
    max_value = max(max(row) for row in mandelbrot_set)
    
    for i in range(height):
        for j in range(width):
            if mandelbrot_set[i][j] == 0:
                f.write('@')
            else:
                index = (len(chars) - 1) * mandelbrot_set[i][j] // max_value
                f.write(chars[index])
        f.write('\n')

print("\nOutput also saved to 'mandelbrot.txt'")
