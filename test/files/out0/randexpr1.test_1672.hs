Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Negate
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Just
                                            ( Expression'Multiply
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
                                                                                                            [ Expression'Minus
                                                                                                                ( Expression'Case
                                                                                                                    ( CaseExpression
                                                                                                                        { base = Nothing
                                                                                                                        , cases =
                                                                                                                            ( Expression'GreaterThan
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "a"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                )
                                                                                                                                ( Expression'BitwiseOr
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
                                                                                                                                    ( Expression'Column
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
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            , Expression'Subquery
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
                                                                                                                                                                        , value = "abs"
                                                                                                                                                                        }
                                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                                        [ Expression'AggregateDistinctFunctionCall
                                                                                                                                                                            ( AggregateDistinctFunctionCallExpression
                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "count"
                                                                                                                                                                                        }
                                                                                                                                                                                    , arguments = Identity
                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "a"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                    }
                                                                                                                                                                                , filter = Nothing
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
                                                                                                                                                , where_ = Nothing
                                                                                                                                                , groupBy = Nothing
                                                                                                                                                , window = Nothing
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    , orderBy = Nothing
                                                                                                                                    , limit = Nothing
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            ) :|
                                                                                                                            [
                                                                                                                                ( Expression'Or
                                                                                                                                    ( Expression'Not
                                                                                                                                        ( Expression'InValues
                                                                                                                                            ( InValuesExpression
                                                                                                                                                { expression = Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "e"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                , values =
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
                                                                                                                                                                                                { value = Expression'Column
                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                        , value = "c"
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
                                                                                                                                                                                            ( Expression'And
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
                                                                                                                                                                                                                        ( Expression'LessThanOrEquals
                                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                    , value = "c"
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                                                                ( Number "11" )
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
                                                                                                                                                                                                ( Expression'GreaterThan
                                                                                                                                                                                                    ( Expression'Column
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
                                                                                                                                                                                                    )
                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "e"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
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
                                                                                                                                                    , Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "d"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    , Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "a"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    ]
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                    ( Expression'NotEquals
                                                                                                                                        ( Expression'Column
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
                                                                                                                                        )
                                                                                                                                        ( Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "e"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                , Expression'Column
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
                                                                                                                            ]
                                                                                                                        , else_ = Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "c"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                                ( Expression'Multiply
                                                                                                                    ( Expression'Negate
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "11" )
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
                                                                                                                    ( Expression'Not
                                                                                                                        ( Expression'InValues
                                                                                                                            ( InValuesExpression
                                                                                                                                { expression = Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "c"
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
                                                                                                                                            , value = "e"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    , Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "d"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    , Expression'LiteralValue
                                                                                                                                        ( Number "19" )
                                                                                                                                    ]
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
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "t1"
                                                                                }
                                                                            )
                                                                        , value = "c"
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
                                                        { namespace = Nothing
                                                        , value = "b"
                                                        }
                                                    )
                                                )
                                            )
                                        , cases =
                                            ( Expression'LiteralValue
                                                ( Number "17" )
                                            , Expression'Column
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
                                            ) :| []
                                        , else_ = Expression'LiteralValue
                                            ( Number "19" )
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
                                    , value = "e"
                                    }
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
                                                                        { value = Expression'Column
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
                                                                                                                    { value = Expression'BitwiseOr
                                                                                                                        ( Expression'Multiply
                                                                                                                            ( Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "c"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            )
                                                                                                                            ( Expression'LiteralValue
                                                                                                                                ( Number "19" )
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "a"
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
                                                                                                                ( Expression'Not
                                                                                                                    ( Expression'And
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
                                                                                                                                                    ( Expression'GreaterThanOrEquals
                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                            ( Number "19" )
                                                                                                                                                        )
                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                            ( Number "13" )
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'And
                                                                                                                                                        ( Expression'And
                                                                                                                                                            ( Expression'Not
                                                                                                                                                                ( Expression'InValues
                                                                                                                                                                    ( InValuesExpression
                                                                                                                                                                        { expression = Expression'Case
                                                                                                                                                                            ( CaseExpression
                                                                                                                                                                                { base = Nothing
                                                                                                                                                                                , cases =
                                                                                                                                                                                    ( Expression'And
                                                                                                                                                                                        ( Expression'GreaterThan
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
                                                                                                                                                                                                                                                        [ Expression'LiteralValue
                                                                                                                                                                                                                                                            ( Number "19" )
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
                                                                                                                                                                                                                                    ( Expression'Or
                                                                                                                                                                                                                                        ( Expression'Equals
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
                                                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                    , value = "d"
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                        ( Expression'NotEquals
                                                                                                                                                                                                                                            ( Expression'Column
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
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                    , value = "e"
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
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
                                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                                    , over = Nothing
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            )
                                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                                ( Number "19" )
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                        ( Expression'Equals
                                                                                                                                                                                            ( Expression'Column
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
                                                                                                                                                                                            )
                                                                                                                                                                                            ( Expression'Column
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
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                    , Expression'Plus
                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                            ( Number "11" )
                                                                                                                                                                                        )
                                                                                                                                                                                        ( Expression'Negate
                                                                                                                                                                                            ( Expression'Column
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
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                    ) :|
                                                                                                                                                                                    [
                                                                                                                                                                                        ( Expression'Not
                                                                                                                                                                                            ( Expression'Between
                                                                                                                                                                                                ( Expression'Negate
                                                                                                                                                                                                    ( Expression'Column
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
                                                                                                                                                                                                    )
                                                                                                                                                                                                )
                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                        }
                                                                                                                                                                                                    )
                                                                                                                                                                                                )
                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                        }
                                                                                                                                                                                                    )
                                                                                                                                                                                                )
                                                                                                                                                                                            )
                                                                                                                                                                                        , Expression'Column
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
                                                                                                                                                                                        )
                                                                                                                                                                                    ]
                                                                                                                                                                                , else_ = Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "d"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
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
                                                                                                                                                                            , Expression'LiteralValue
                                                                                                                                                                                ( Number "13" )
                                                                                                                                                                            , Expression'Column
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
                                                                                                                                                                            ]
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'InSubquery
                                                                                                                                                                ( InSubqueryExpression
                                                                                                                                                                    { expression = Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
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
                                                                                                                                                                                                { value = Expression'LiteralValue
                                                                                                                                                                                                    ( Number "13" )
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
                                                                                                                                                                                                    , value = "f"
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
                                                                                                                                                        ( Expression'LessThan
                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                ( Number "17" )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "f"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
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
                                                                                                                        ( Expression'NotEquals
                                                                                                                            ( Expression'Column
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
                                                                                                                            )
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
                                                                                                { namespace = Just
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "t1"
                                                                                                        }
                                                                                                    )
                                                                                                , value = "d"
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
                                    , filter = Nothing
                                    , over = Nothing
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