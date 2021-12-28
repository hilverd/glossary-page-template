const defaultTheme = require('tailwindcss/defaultTheme')

module.exports = {
  content: [
    './src/**/*.elm',
    './glossary.{css,html,js}'
  ],
  plugins: [
    require('@tailwindcss/forms'),
  ],
  theme: {
    extend: {
      screens: {
        'print': { 'raw': 'print' },
      },
      fontFamily: {
        sans: ['Overpass', ...defaultTheme.fontFamily.sans],
      },
    },
  },
}
