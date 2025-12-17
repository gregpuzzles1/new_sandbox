import pygame
import random
import sys
import threading

# Initialize pygame
pygame.init()

# Screen settings
WIDTH, HEIGHT = 800, 600
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Tribute to Hemp - The Ultimate Fetch Champion")

# Colors
WHITE = (255, 255, 255)
GREEN = (34, 139, 34)

# Load images
ball_img = pygame.image.load("assets/Tennis_ball.svg")
ball_img = pygame.transform.scale(ball_img, (50, 50))

dog_font = pygame.font.Font(None, 36)
tribute_texts = [
    "Hemp, the Border Collie Extraordinaire!",
    "Fast, smart, and always ready to catch a ball!",
    "A loyal friend with a heart of gold!",
    "No tennis ball is safe when Hemp is around!",
    "The ultimate fetch champion of all time!",
]

# Ball movement
ball_x, ball_y = random.randint(100, 700), random.randint(100, 500)
ball_dx, ball_dy = 5, 5

text_index = 0
text_timer = 0
text_delay = 100  # Delay in frames before changing text

running = True

# Function to listen for Enter key in a separate thread
def listen_for_exit():
    global running
    sys.stdin.read(1)  # Wait for a single key press (Enter)
    running = False

# Start listening for Enter key in the background
threading.Thread(target=listen_for_exit, daemon=True).start()

# Main loop
while running:
    screen.fill(GREEN)  # Background representing a grassy field

    # Update text timer
    text_timer += 1
    if text_timer > text_delay:
        text_timer = 0
        text_index = (text_index + 1) % len(tribute_texts)

    # Draw tribute text
    text_surface = dog_font.render(tribute_texts[text_index], True, WHITE)
    screen.blit(text_surface, (WIDTH//2 - text_surface.get_width()//2, 50))

    # Move ball
    ball_x += ball_dx
    ball_y += ball_dy

    # Bounce off walls
    if ball_x <= 0 or ball_x + 50 >= WIDTH:
        ball_dx = -ball_dx
    if ball_y <= 0 or ball_y + 50 >= HEIGHT:
        ball_dy = -ball_dy

    # Draw ball
    screen.blit(ball_img, (ball_x, ball_y))

    pygame.display.flip()
    pygame.time.delay(30)  # Smooth animation

pygame.quit()
