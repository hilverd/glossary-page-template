const defaultTheme = require('tailwindcss/defaultTheme')

module.exports = {
  content: [
    './index.html',
    './glossary.{css,html,js,ts}',
    './src/**/*.elm'
  ],
  plugins: [
    require('@tailwindcss/forms'),
    require('@tailwindcss/typography'),
  ],
  darkMode: 'class',
  theme: {
    extend: {
      screens: {
        '3xl': '1800px',
        'print': { 'raw': 'print' },
      },
      fontFamily: {
        sans: ['"Source Sans 3"', ...defaultTheme.fontFamily.sans],
      },
    },
  },
}
