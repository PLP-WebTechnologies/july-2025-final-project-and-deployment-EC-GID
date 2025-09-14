// Modal functionality
document.addEventListener('DOMContentLoaded', () => {
  const modalLinks = document.querySelectorAll('.modal-link');
  const modals = document.querySelectorAll('.modal');
  const closeButtons = document.querySelectorAll('.close');

  // Open modal
  modalLinks.forEach(link => {
    link.addEventListener('click', (e) => {
      e.preventDefault();
      const modalId = link.getAttribute('data-modal');
      document.getElementById(modalId).classList.remove('hidden');
    });
  });

  // Close modal with X
  closeButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      btn.closest('.modal').classList.add('hidden');
    });
  });

  // Close modal when clicking outside
  modals.forEach(modal => {
    modal.addEventListener('click', (e) => {
      if (e.target === modal) {
        modal.classList.add('hidden');
      }
    });
  });
});