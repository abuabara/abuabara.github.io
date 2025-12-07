// ===============================
// Carousel State
// ===============================
let currentIndex = 0;
const slides = document.querySelectorAll(".carousel-images img");
const captionBox = document.getElementById("carouselCaption");
const totalSlides = slides.length;

// ===============================
// Initialize Carousel
// ===============================
function initializeCarousel() {
    slides.forEach((img, i) => {
        img.style.position = "absolute";
        img.style.top = "0";
        img.style.left = "0";
        img.style.width = "100%";
        img.style.transition = "opacity 0.6s ease";
        img.style.opacity = i === 0 ? "1" : "0";   // Show first image
    });

    updateCaption();
}

initializeCarousel();

// ===============================
// Update Caption
// ===============================
function updateCaption() {
    const caption = slides[currentIndex].dataset.caption || "";
    captionBox.textContent = caption;
}

// ===============================
// Show Slide
// ===============================
function showSlide(newIndex) {
    slides[currentIndex].style.opacity = "0";   // fade out current
    slides[newIndex].style.opacity = "1";       // fade in next

    currentIndex = newIndex;
    updateCaption();
}

// ===============================
// Next/Previous Slide
// ===============================
function changeSlide(direction) {
    const newIndex = (currentIndex + direction + totalSlides) % totalSlides;
    showSlide(newIndex);
}

// ===============================
// Keyboard Navigation
// ===============================
document.addEventListener("keydown", (e) => {
    if (e.key === "ArrowRight") changeSlide(1);
    if (e.key === "ArrowLeft") changeSlide(-1);
});

// ===============================
// Touch Swipe Support
// ===============================
let startX = 0;

const container = document.querySelector(".carousel-images");

container.addEventListener("touchstart", (e) => {
    startX = e.touches[0].clientX;
});

container.addEventListener("touchend", (e) => {
    const endX = e.changedTouches[0].clientX;
    const diff = startX - endX;

    if (Math.abs(diff) > 50) {
        if (diff > 0) changeSlide(1);      // swipe left → next
        else changeSlide(-1);              // swipe right → previous
    }
});