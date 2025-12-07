let currentIndex = 0; // Track the current slide index
const images = document.querySelectorAll('.carousel-images img'); // Select all images in the carousel
const totalImages = images.length; // Get the total number of images

// Function to change the slide
function changeSlide(direction) {
    // Hide the current image
    images[currentIndex].style.display = 'none';

    // Update the current index based on the direction
    currentIndex = (currentIndex + direction + totalImages) % totalImages;

    // Show the new current image
    images[currentIndex].style.display = 'block';
}

// Initialize the carousel by displaying the first image
function initializeCarousel() {
    images.forEach((img, index) => {
        img.style.display = index === currentIndex ? 'block' : 'none'; // Show only the current image
    });
}

// Start the carousel
initializeCarousel();