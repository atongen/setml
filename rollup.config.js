import node_resolve from 'rollup-plugin-node-resolve';
import uglify from 'rollup-plugin-uglify';

export default {
    input: './src/Main.bs.js',
    output: {
        file: './release/main.js',
        format: 'iife'
    },
    plugins: [
        node_resolve({module: true, browser: true}),
        uglify()
    ],
    name: 'setml',
    sourceMap: true
}
