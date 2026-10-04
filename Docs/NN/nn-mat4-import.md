# Neural-network MAT4 import

`TNNMAT4Importer` reads the PaNDA sequential-network MAT4 format. It does not parse native Keras or Mathematica files; an exporter must write the manifest and parameter matrices described here.

## MAT4 matrices

1. The first matrix is named `__panda_nn__`. It is a one-dimensional `uint8` array containing UTF-8 JSON.
2. Remaining matrices contain named real-valued network parameters. Names referenced by the manifest must be unique. The importer converts parameter values to `Single`.

The importer supports dense, convolution, and 1D/2D max-pooling layers, plus `relu`/`ramp`, `sigmoid`, and `softmax` activation layers. Complex parameters are rejected.

## Manifest version 1

```json
{
  "format": "panda.nn.mat4",
  "version": 1,
  "input_shape": [2],
  "layers": [
    {
      "type": "dense",
      "weights": {
        "kernel": "dense.kernel",
        "bias": "dense.bias"
      },
      "kernel_layout": "input_output",
      "activation": "sigmoid"
    }
  ]
}
```

`input_shape` contains positive dimensions, excluding a batch dimension. Layers are applied in array order. A dense `kernel` must be a rank-2 MAT4 matrix. Its default layout, `input_output` (also `io`), matches Keras; the importer transposes it to the runtime's `[output, input]` layout. Set `kernel_layout` to `output_input` (or `oi`) to use the runtime layout without transposition. `bias` is optional and, when present, must name a rank-1 matrix.

Dense `activation` is optional and defaults to `linear`. Supported values are `linear`, `none`, `relu`, `ramp`, `sigmoid`, and `softmax`. Activation-only layers can also be listed with `type` set to one of `relu`, `ramp`, `sigmoid`, or `softmax`.

A flatten layer uses `type: "flatten"`. Its optional `level` defaults to `"Infinity"`, which combines all input axes into one vector. A nonnegative integer combines the first `level + 1` axes; a negative integer combines the last `abs(level) + 1` axes, following Mathematica's `FlattenLayer[n]` convention. Levels that select more axes than the input rank are rejected during network initialization.

For a softmax activation-only layer, optional `level` specifies the normalization level and defaults to `-1` (the innermost dimension), matching Mathematica's `SoftmaxLayer`. A dense layer that requests `activation: "softmax"` can set the same property using `activation_level`.

A convolution layer uses `type: "conv"`, a rank-2 MAT4 `kernel` matrix, optional rank-1 `bias`, and a `kernel_shape` array giving the runtime tensor shape. `kernel_shape` must have rank 3 for 1D convolution (`[filters, input_channels, kernel_width]`) or rank 4 for 2D convolution (`[filters, input_channels, kernel_height, kernel_width]`); its product must equal the number of matrix elements. Kernel values are reshaped in storage order before constructing `TConvLayer`.

A max-pooling layer uses `type: "maxpool"` with `pool_size` as a one- or two-element array. `stride` is optional; when omitted, it defaults to `pool_size`. Padding and non-max pooling functions are not represented by this format. The importer creates a `TPoolingLayer` for each such entry.

Optional `input_encoder` metadata creates a chain-owned image encoder:

```json
"input_encoder": {
  "type": "image",
  "image_size": [28, 28],
  "color_space": "grayscale",
  "interleaving": false,
  "data_transposed": false
}
```

`image_size` is `[width, height]` and defaults to `[128, 128]`. `color_space` is `rgb` (default) or `grayscale`. Image resizing uses bilinear interpolation with stretch behavior, and 8-bit pixels are normalized to `[0, 1]`. The default output is channel-first (`[channels, height, width]`); `interleaving` places channels in the final axis, and `data_transposed` swaps width and height. Supported source images are 8-bit grayscale and RGB24.

Optional `output_decoder` metadata creates a chain-owned class decoder:

```json
"output_decoder": {
  "type": "class",
  "labels": [0, 1, 2, 3]
}
```

`labels` is optional. The decoder treats the final input axis as classes and returns the label at the first maximum. Rank-1 input returns one label; higher-rank input returns a flattened array of labels for each preceding index. With no labels, it returns zero-based maximum indices. String and numeric JSON labels retain their value types.

`TNNetChain` owns the encoder and decoder assigned through `InputEncoder` and `OutputDecoder`. Encode external images with `InputEncoder.Encode(image)`, execute the resulting tensor, and call `OutputDecoder.Decode(network.Output)` for the decoded result. Both adapters may be absent.

## Importing

Create `TNNMAT4Importer` with a file name or stream, call `ReadNetwork`, then free the importer and the returned caller-owned `TNNetChain` when finished. The importer initializes the chain using `input_shape`; malformed manifests, missing parameters, unsupported layers, and incompatible shapes raise `ENNImportError`.
