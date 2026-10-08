# Neural Network Export and Native Evaluation

Neural networks can be trained using an external machine-learning framework such as Mathematica. After training, the network is exported to a platform-independent representation containing the network structure and its numerical parameters.

The PaNDA application imports this representation and evaluates the network using its own native Delphi implementation of the required neural-network layers. The trained model therefore does not require Mathematica or another machine-learning framework at runtime.

The current workflow is:

**Training → Export → Import → Native inference**

For example:

1. A neural network is created and trained in Mathematica.
2. The trained network is exported to a MAT4 file.
3. PaNDA imports the network structure, layer parameters, and weights from the MAT4 file.
4. The imported network is represented by native Delphi objects.
5. PaNDA evaluates the network using its own implementations of the supported layers.
6. The numerical results are verified against the original Mathematica network.

This approach separates **model training** from **model execution**. Mathematica is used as a training and model-development environment, while the deployed application only requires the PaNDA runtime.

The MAT4 file is used as a model exchange format rather than as a runtime dependency on MATLAB or Mathematica.

# Layer Compatibility

The native evaluator implements a subset of the layers and options available in the original machine-learning framework. When a network uses only supported operations, its native evaluation should produce the same results as the original network, within the expected numerical precision.

Additional layer types and layer options can be implemented as required.

Consequently, compatibility is defined at the level of the network operations rather than by attempting to reproduce the complete Mathematica neural-network framework.
