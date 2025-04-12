#!/usr/bin/env ruby

def mandelbrot(h, w, max_iter)
  # Define the region of the complex plane to plot
  y_min, y_max = -1.2, 1.2
  x_min, x_max = -2.0, 1.0
  
  # Initialize the output array
  result = Array.new(h) { Array.new(w, 0) }
  
  # For each pixel in the output image
  (0...h).each do |i|
    (0...w).each do |j|
      # Convert pixel coordinates to complex plane coordinates
      x = x_min + j * (x_max - x_min) / (w - 1)
      y = y_min + i * (y_max - y_min) / (h - 1)
      
      # The complex number to test
      c_real, c_imag = x, y
      z_real, z_imag = 0.0, 0.0
      
      # Iterate to see if the point escapes
      max_iter.times do |k|
        # Calculate z = z^2 + c manually without Complex class
        # (a+bi)^2 = a^2 - b^2 + 2abi
        z_real_temp = z_real*z_real - z_imag*z_imag + c_real
        z_imag = 2*z_real*z_imag + c_imag
        z_real = z_real_temp
        
        # If the point escapes (|z| > 2)
        if z_real*z_real + z_imag*z_imag > 4
          # Color is based on how quickly the point escapes
          result[i][j] = k + 1
          break
        end
      end
      
      # If the point doesn't escape, it's in the Mandelbrot set
      # We leave it as 0 in the result
    end
  end
  
  return result
end

def print_mandelbrot(mandelbrot_set)
  h = mandelbrot_set.length
  w = mandelbrot_set[0].length
  
  # Define a set of characters with increasing "density"
  chars = ' .:-=+*#%@'
  
  # Find the maximum value in the set for normalization
  max_value = mandelbrot_set.flatten.max
  
  (0...h).each do |i|
    (0...w).each do |j|
      if mandelbrot_set[i][j] == 0
        # Points in the set
        print '@'
      else
        # Points outside the set, with a character representing how quickly they escape
        index = (chars.length - 1) * mandelbrot_set[i][j] / max_value
        print chars[index.to_i]
      end
    end
    puts  # New line after each row
  end
end

# Set parameters
height = 120
width = 100
max_iterations = 1000

# Generate and print the Mandelbrot set
puts "Generating Mandelbrot set..."
mandelbrot_set = mandelbrot(height, width, max_iterations)
puts "Printing Mandelbrot set:"
print_mandelbrot(mandelbrot_set)

# For a better view, let's also save the output to a file
puts "\nSaving output to 'mandelbrot.txt'..."
File.open('mandelbrot.txt', 'w') do |f|
  chars = ' .:-=+*#%@'
  max_value = mandelbrot_set.flatten.max
  
  (0...height).each do |i|
    (0...width).each do |j|
      if mandelbrot_set[i][j] == 0
        f.write('@')
      else
        index = (chars.length - 1) * mandelbrot_set[i][j] / max_value
        f.write(chars[index.to_i])
      end
    end
    f.write("\n")
  end
end

puts "Done!"
