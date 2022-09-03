def run_DML_OF_c(
    Y,
    T,
    X,
    W,
    X_test,
    n_trees=1000,
    min_leaf_size=10,
    max_depth=30,
    subsample_ratio=0.7,
):

    est = DMLOrthoForest(
        model_Y=WeightedLassoCVWrapper(cv=3),
        model_T=WeightedLassoCVWrapper(cv=3),
        model_Y_final=GradientBoostingRegressor(
            n_estimators=300, max_depth=20, min_samples_leaf=10
        ),
        model_T_final=MultiOutputRegressor(
            GradientBoostingRegressor(
                n_estimators=300, max_depth=20, min_samples_leaf=10
            )
        ),
        n_trees=n_trees,
        min_leaf_size=min_leaf_size,
        max_depth=max_depth,
        subsample_ratio=subsample_ratio,
    )

    est.fit(Y, T, X=X, W=W)

    treatment_effects = est.const_marginal_effect(X_test)
    # treatment_effects = est.effect(X_test)

    return treatment_effects
