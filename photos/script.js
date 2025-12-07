/*
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
*/

// =============================
// Carousel State
// =============================
let currentIndex = 0;
const images = document.querySelectorAll('.carousel-images img');
const totalImages = images.length;

// =============================
// Initialize
// =============================
function initializeCarousel() {
    images.forEach((img, idx) => {
        img.style.opacity = idx === 0 ? "1" : "0";
        img.style.position = "absolute";
        img.style.top = "0";
        img.style.left = "0";
        img.style.transition = "opacity 0.6s ease";
    });
}

initializeCarousel();

// =============================
// Show Slide
// =============================
function showSlide(newIndex) {
    images[currentIndex].style.opacity = "0";
    images[newIndex].style.opacity = "1";
    currentIndex = newIndex;
}

// =============================
// Next/Previous Slide
// =============================
function changeSlide(direction) {
    const newIndex = (currentIndex + direction + totalImages) % totalImages;
    showSlide(newIndex);
}

// =============================
// Keyboard Navigation
// =============================
document.addEventListener("keydown", (e) => {
    if (e.key === "ArrowRight") changeSlide(1);
    if (e.key === "ArrowLeft") changeSlide(-1);
});

// =============================
// Touch Swipe Support
// =============================
let startX = 0;

document.querySelector('.carousel-images').addEventListener("touchstart", (e) => {
    startX = e.touches[0].clientX;
});

document.querySelector('.carousel-images').addEventListener("touchend", (e) => {
    const endX = e.changedTouches[0].clientX;
    const diff = startX - endX;

    if (Math.abs(diff) > 50) {
        if (diff > 0) changeSlide(1);   // swipe left → next
        else changeSlide(-1);           // swipe right → previous
    }
});