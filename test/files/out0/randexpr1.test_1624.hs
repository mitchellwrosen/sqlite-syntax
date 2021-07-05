Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'BitwiseAnd
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "coalesce"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Subquery
                                                    ( SelectStatement
                                                        { commonTableExpressions = Nothing
                                                        , select = CompoundSelect
                                                            ( SelectCore'Select
                                                                ( Select
                                                                    { distinct = False
                                                                    , columns = ResultColumn'Expression
                                                                        ( Aliased
                                                                            { value = Expression'Case
                                                                                ( CaseExpression
                                                                                    { base = Nothing
                                                                                    , cases =
                                                                                        ( Expression'InSubquery
                                                                                            ( InSubqueryExpression
                                                                                                { expression = Expression'FunctionCall
                                                                                                    ( FunctionCallExpression
                                                                                                        { call = FunctionCall
                                                                                                            { name = Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "coalesce"
                                                                                                                }
                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                [ Expression'Subquery
                                                                                                                    ( SelectStatement
                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                        , select = CompoundSelect
                                                                                                                            ( SelectCore'Select
                                                                                                                                ( Select
                                                                                                                                    { distinct = False
                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                        ( Aliased
                                                                                                                                            { value = Expression'FunctionCall
                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                    { call = FunctionCall
                                                                                                                                                        { name = Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "max"
                                                                                                                                                            }
                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                            [ Expression'FunctionCall
                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "coalesce"
                                                                                                                                                                            }
                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                            [ Expression'Subquery
                                                                                                                                                                                ( SelectStatement
                                                                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                                                                    , select = CompoundSelect
                                                                                                                                                                                        ( SelectCore'Select
                                                                                                                                                                                            ( Select
                                                                                                                                                                                                { distinct = False
                                                                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                                                                    ( Aliased
                                                                                                                                                                                                        { value = Expression'FunctionCall
                                                                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                        , value = "max"
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                        [ Expression'Minus
                                                                                                                                                                                                                            ( Expression'Minus
                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                        , value = "e"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                        , value = "c"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                            ( Expression'FunctionCall
                                                                                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                            , value = "coalesce"
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                            [ Expression'Subquery
                                                                                                                                                                                                                                                ( SelectStatement
                                                                                                                                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                                                                                                                                    , select = CompoundSelect
                                                                                                                                                                                                                                                        ( SelectCore'Select
                                                                                                                                                                                                                                                            ( Select
                                                                                                                                                                                                                                                                { distinct = False
                                                                                                                                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                                                                                                                                    ( Aliased
                                                                                                                                                                                                                                                                        { value = Expression'FunctionCall
                                                                                                                                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                                                        , value = "max"
                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                                                        [ Expression'Column
                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                , value = "c"
                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                , filter = Nothing
                                                                                                                                                                                                                                                                                , over = Nothing
                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                        , alias = Nothing
                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                    ) :| []
                                                                                                                                                                                                                                                                , from = Just
                                                                                                                                                                                                                                                                    ( Table
                                                                                                                                                                                                                                                                        ( QualifiedTableName
                                                                                                                                                                                                                                                                            { name = Aliased
                                                                                                                                                                                                                                                                                { value = Namespaced
                                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                , alias = Nothing
                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                            , indexedBy = Nothing
                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                , where_ = Just
                                                                                                                                                                                                                                                                    ( Expression'Between
                                                                                                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                                                                                                            ( Number "19" )
                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                                                                                                            ( Number "13" )
                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                                                                                                            ( Number "13" )
                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                , groupBy = Nothing
                                                                                                                                                                                                                                                                , window = Nothing
                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                    , orderBy = Nothing
                                                                                                                                                                                                                                                    , limit = Nothing
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            , Expression'Plus
                                                                                                                                                                                                                                                ( Expression'Divide
                                                                                                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                    , value = "abs"
                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                                    [ Expression'Case
                                                                                                                                                                                                                                                                        ( CaseExpression
                                                                                                                                                                                                                                                                            { base = Nothing
                                                                                                                                                                                                                                                                            , cases =
                                                                                                                                                                                                                                                                                ( Expression'Or
                                                                                                                                                                                                                                                                                    ( Expression'LessThanOrEquals
                                                                                                                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                , value = "c"
                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                                                                                                                            ( Number "17" )
                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                    ( Expression'InValues
                                                                                                                                                                                                                                                                                        ( InValuesExpression
                                                                                                                                                                                                                                                                                            { expression = Expression'Column
                                                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                                                    , value = "f"
                                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                                            , values =
                                                                                                                                                                                                                                                                                                [ Expression'Column
                                                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                                        , value = "a"
                                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                , Expression'Negate
                                                                                                                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                                                                                                                        ( Number "17" )
                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                                                        , value = "b"
                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                ) :| []
                                                                                                                                                                                                                                                                            , else_ = Expression'Column
                                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                    , value = "c"
                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                                                                                                            , over = Nothing
                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                    , value = "abs"
                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                                    [ Expression'Column
                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                            , value = "e"
                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                                                                                                            , over = Nothing
                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                        , value = "b"
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                                                                    , over = Nothing
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                , filter = Nothing
                                                                                                                                                                                                                , over = Nothing
                                                                                                                                                                                                                }
                                                                                                                                                                                                            )
                                                                                                                                                                                                        , alias = Nothing
                                                                                                                                                                                                        }
                                                                                                                                                                                                    ) :| []
                                                                                                                                                                                                , from = Just
                                                                                                                                                                                                    ( Table
                                                                                                                                                                                                        ( QualifiedTableName
                                                                                                                                                                                                            { name = Aliased
                                                                                                                                                                                                                { value = Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                , alias = Nothing
                                                                                                                                                                                                                }
                                                                                                                                                                                                            , indexedBy = Nothing
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    )
                                                                                                                                                                                                , where_ = Just
                                                                                                                                                                                                    ( Expression'Not
                                                                                                                                                                                                        ( Expression'Exists
                                                                                                                                                                                                            ( SelectStatement
                                                                                                                                                                                                                { commonTableExpressions = Nothing
                                                                                                                                                                                                                , select = CompoundSelect
                                                                                                                                                                                                                    ( SelectCore'Select
                                                                                                                                                                                                                        ( Select
                                                                                                                                                                                                                            { distinct = False
                                                                                                                                                                                                                            , columns = ResultColumn'Expression
                                                                                                                                                                                                                                ( Aliased
                                                                                                                                                                                                                                    { value = Expression'LiteralValue
                                                                                                                                                                                                                                        ( Number "1" )
                                                                                                                                                                                                                                    , alias = Nothing
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                ) :| []
                                                                                                                                                                                                                            , from = Just
                                                                                                                                                                                                                                ( Table
                                                                                                                                                                                                                                    ( QualifiedTableName
                                                                                                                                                                                                                                        { name = Aliased
                                                                                                                                                                                                                                            { value = Namespaced
                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            , alias = Nothing
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        , indexedBy = Nothing
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            , where_ = Just
                                                                                                                                                                                                                                ( Expression'GreaterThanOrEquals
                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            , value = "e"
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                            , value = "b"
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            , groupBy = Nothing
                                                                                                                                                                                                                            , window = Nothing
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                , orderBy = Nothing
                                                                                                                                                                                                                , limit = Nothing
                                                                                                                                                                                                                }
                                                                                                                                                                                                            )
                                                                                                                                                                                                        )
                                                                                                                                                                                                    )
                                                                                                                                                                                                , groupBy = Nothing
                                                                                                                                                                                                , window = Nothing
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                    , orderBy = Nothing
                                                                                                                                                                                    , limit = Nothing
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            , Expression'Column
                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "e"
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            ]
                                                                                                                                                                        }
                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                    , over = Nothing
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            ]
                                                                                                                                                        }
                                                                                                                                                    , filter = Nothing
                                                                                                                                                    , over = Nothing
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            , alias = Nothing
                                                                                                                                            }
                                                                                                                                        ) :| []
                                                                                                                                    , from = Just
                                                                                                                                        ( Table
                                                                                                                                            ( QualifiedTableName
                                                                                                                                                { name = Aliased
                                                                                                                                                    { value = Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "t1"
                                                                                                                                                        }
                                                                                                                                                    , alias = Nothing
                                                                                                                                                    }
                                                                                                                                                , indexedBy = Nothing
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    , where_ = Just
                                                                                                                                        ( Expression'Not
                                                                                                                                            ( Expression'Exists
                                                                                                                                                ( SelectStatement
                                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                                    , select = CompoundSelect
                                                                                                                                                        ( SelectCore'Select
                                                                                                                                                            ( Select
                                                                                                                                                                { distinct = False
                                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                                    ( Aliased
                                                                                                                                                                        { value = Expression'LiteralValue
                                                                                                                                                                            ( Number "1" )
                                                                                                                                                                        , alias = Nothing
                                                                                                                                                                        }
                                                                                                                                                                    ) :| []
                                                                                                                                                                , from = Just
                                                                                                                                                                    ( Table
                                                                                                                                                                        ( QualifiedTableName
                                                                                                                                                                            { name = Aliased
                                                                                                                                                                                { value = Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                    }
                                                                                                                                                                                , alias = Nothing
                                                                                                                                                                                }
                                                                                                                                                                            , indexedBy = Nothing
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                , where_ = Just
                                                                                                                                                                    ( Expression'Or
                                                                                                                                                                        ( Expression'LessThanOrEquals
                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                ( Number "13" )
                                                                                                                                                                            )
                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                            }
                                                                                                                                                                                        )
                                                                                                                                                                                    , value = "b"
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            )
                                                                                                                                                                        )
                                                                                                                                                                        ( Expression'InSubquery
                                                                                                                                                                            ( InSubqueryExpression
                                                                                                                                                                                { expression = Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                , subquery = SelectStatement
                                                                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                                                                    , select = Union
                                                                                                                                                                                        ( CompoundSelect
                                                                                                                                                                                            ( SelectCore'Select
                                                                                                                                                                                                ( Select
                                                                                                                                                                                                    { distinct = False
                                                                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                                                                        ( Aliased
                                                                                                                                                                                                            { value = Expression'Column
                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "d"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                )
                                                                                                                                                                                                            , alias = Nothing
                                                                                                                                                                                                            }
                                                                                                                                                                                                        ) :| []
                                                                                                                                                                                                    , from = Just
                                                                                                                                                                                                        ( Table
                                                                                                                                                                                                            ( QualifiedTableName
                                                                                                                                                                                                                { name = Aliased
                                                                                                                                                                                                                    { value = Namespaced
                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                        , value = "t1"
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    , alias = Nothing
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                , indexedBy = Nothing
                                                                                                                                                                                                                }
                                                                                                                                                                                                            )
                                                                                                                                                                                                        )
                                                                                                                                                                                                    , where_ = Nothing
                                                                                                                                                                                                    , groupBy = Nothing
                                                                                                                                                                                                    , window = Nothing
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                        ( SelectCore'Select
                                                                                                                                                                                            ( Select
                                                                                                                                                                                                { distinct = False
                                                                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                                                                    ( Aliased
                                                                                                                                                                                                        { value = Expression'Column
                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                { namespace = Just
                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                        , value = "t1"
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                , value = "d"
                                                                                                                                                                                                                }
                                                                                                                                                                                                            )
                                                                                                                                                                                                        , alias = Nothing
                                                                                                                                                                                                        }
                                                                                                                                                                                                    ) :| []
                                                                                                                                                                                                , from = Just
                                                                                                                                                                                                    ( Table
                                                                                                                                                                                                        ( QualifiedTableName
                                                                                                                                                                                                            { name = Aliased
                                                                                                                                                                                                                { value = Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                , alias = Nothing
                                                                                                                                                                                                                }
                                                                                                                                                                                                            , indexedBy = Nothing
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    )
                                                                                                                                                                                                , where_ = Nothing
                                                                                                                                                                                                , groupBy = Nothing
                                                                                                                                                                                                , window = Nothing
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                    , orderBy = Nothing
                                                                                                                                                                                    , limit = Nothing
                                                                                                                                                                                    }
                                                                                                                                                                                }
                                                                                                                                                                            )
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                , groupBy = Nothing
                                                                                                                                                                , window = Nothing
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    , orderBy = Nothing
                                                                                                                                                    , limit = Nothing
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    , groupBy = Nothing
                                                                                                                                    , window = Nothing
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            )
                                                                                                                        , orderBy = Nothing
                                                                                                                        , limit = Nothing
                                                                                                                        }
                                                                                                                    )
                                                                                                                , Expression'LiteralValue
                                                                                                                    ( Number "17" )
                                                                                                                ]
                                                                                                            }
                                                                                                        , filter = Nothing
                                                                                                        , over = Nothing
                                                                                                        }
                                                                                                    )
                                                                                                , subquery = SelectStatement
                                                                                                    { commonTableExpressions = Nothing
                                                                                                    , select = Union
                                                                                                        ( CompoundSelect
                                                                                                            ( SelectCore'Select
                                                                                                                ( Select
                                                                                                                    { distinct = False
                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                        ( Aliased
                                                                                                                            { value = Expression'LiteralValue
                                                                                                                                ( Number "11" )
                                                                                                                            , alias = Nothing
                                                                                                                            }
                                                                                                                        ) :| []
                                                                                                                    , from = Just
                                                                                                                        ( Table
                                                                                                                            ( QualifiedTableName
                                                                                                                                { name = Aliased
                                                                                                                                    { value = Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    , alias = Nothing
                                                                                                                                    }
                                                                                                                                , indexedBy = Nothing
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    , where_ = Nothing
                                                                                                                    , groupBy = Nothing
                                                                                                                    , window = Nothing
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        )
                                                                                                        ( SelectCore'Select
                                                                                                            ( Select
                                                                                                                { distinct = False
                                                                                                                , columns = ResultColumn'Expression
                                                                                                                    ( Aliased
                                                                                                                        { value = Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Just
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , value = "d"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , alias = Nothing
                                                                                                                        }
                                                                                                                    ) :| []
                                                                                                                , from = Just
                                                                                                                    ( Table
                                                                                                                        ( QualifiedTableName
                                                                                                                            { name = Aliased
                                                                                                                                { value = Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "t1"
                                                                                                                                    }
                                                                                                                                , alias = Nothing
                                                                                                                                }
                                                                                                                            , indexedBy = Nothing
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                , where_ = Nothing
                                                                                                                , groupBy = Nothing
                                                                                                                , window = Nothing
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    , orderBy = Nothing
                                                                                                    , limit = Nothing
                                                                                                    }
                                                                                                }
                                                                                            )
                                                                                        , Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "c"
                                                                                                }
                                                                                            )
                                                                                        ) :| []
                                                                                    , else_ = Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Just
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "t1"
                                                                                                    }
                                                                                                )
                                                                                            , value = "e"
                                                                                            }
                                                                                        )
                                                                                    }
                                                                                )
                                                                            , alias = Nothing
                                                                            }
                                                                        ) :| []
                                                                    , from = Just
                                                                        ( Table
                                                                            ( QualifiedTableName
                                                                                { name = Aliased
                                                                                    { value = Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "t1"
                                                                                        }
                                                                                    , alias = Nothing
                                                                                    }
                                                                                , indexedBy = Nothing
                                                                                }
                                                                            )
                                                                        )
                                                                    , where_ = Just
                                                                        ( Expression'InSubquery
                                                                            ( InSubqueryExpression
                                                                                { expression = Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Just
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "t1"
                                                                                                }
                                                                                            )
                                                                                        , value = "c"
                                                                                        }
                                                                                    )
                                                                                , subquery = SelectStatement
                                                                                    { commonTableExpressions = Nothing
                                                                                    , select = Union
                                                                                        ( CompoundSelect
                                                                                            ( SelectCore'Select
                                                                                                ( Select
                                                                                                    { distinct = False
                                                                                                    , columns = ResultColumn'Expression
                                                                                                        ( Aliased
                                                                                                            { value = Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "b"
                                                                                                                    }
                                                                                                                )
                                                                                                            , alias = Nothing
                                                                                                            }
                                                                                                        ) :| []
                                                                                                    , from = Just
                                                                                                        ( Table
                                                                                                            ( QualifiedTableName
                                                                                                                { name = Aliased
                                                                                                                    { value = Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "t1"
                                                                                                                        }
                                                                                                                    , alias = Nothing
                                                                                                                    }
                                                                                                                , indexedBy = Nothing
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    , where_ = Nothing
                                                                                                    , groupBy = Nothing
                                                                                                    , window = Nothing
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                        ( SelectCore'Select
                                                                                            ( Select
                                                                                                { distinct = False
                                                                                                , columns = ResultColumn'Expression
                                                                                                    ( Aliased
                                                                                                        { value = Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "b"
                                                                                                                }
                                                                                                            )
                                                                                                        , alias = Nothing
                                                                                                        }
                                                                                                    ) :| []
                                                                                                , from = Just
                                                                                                    ( Table
                                                                                                        ( QualifiedTableName
                                                                                                            { name = Aliased
                                                                                                                { value = Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "t1"
                                                                                                                    }
                                                                                                                , alias = Nothing
                                                                                                                }
                                                                                                            , indexedBy = Nothing
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                , where_ = Nothing
                                                                                                , groupBy = Nothing
                                                                                                , window = Nothing
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                    , orderBy = Nothing
                                                                                    , limit = Nothing
                                                                                    }
                                                                                }
                                                                            )
                                                                        )
                                                                    , groupBy = Nothing
                                                                    , window = Nothing
                                                                    }
                                                                )
                                                            )
                                                        , orderBy = Nothing
                                                        , limit = Nothing
                                                        }
                                                    )
                                                , Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "f"
                                                        }
                                                    )
                                                ]
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                )
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "t1"
                                                }
                                            )
                                        , value = "c"
                                        }
                                    )
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "t1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'LessThanOrEquals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "c"
                                    }
                                )
                            )
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "e"
                                    }
                                )
                            )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )