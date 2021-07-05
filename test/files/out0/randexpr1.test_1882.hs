Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Minus
                                ( Expression'Negate
                                    ( Expression'Minus
                                        ( Expression'LiteralValue
                                            ( Number "17" )
                                        )
                                        ( Expression'Multiply
                                            ( Expression'Case
                                                ( CaseExpression
                                                    { base = Just
                                                        ( Expression'Subquery
                                                            ( SelectStatement
                                                                { commonTableExpressions = Nothing
                                                                , select = CompoundSelect
                                                                    ( SelectCore'Select
                                                                        ( Select
                                                                            { distinct = False
                                                                            , columns = ResultColumn'Expression
                                                                                ( Aliased
                                                                                    { value = Expression'Minus
                                                                                        ( Expression'Minus
                                                                                            ( Expression'Plus
                                                                                                ( Expression'Plus
                                                                                                    ( Expression'FunctionCall
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
                                                                                                                            , value = "a"
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
                                                                                                                    , value = "min"
                                                                                                                    }
                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                    [ Expression'Negate
                                                                                                                        ( Expression'Divide
                                                                                                                            ( Expression'FunctionCall
                                                                                                                                ( FunctionCallExpression
                                                                                                                                    { call = FunctionCall
                                                                                                                                        { name = Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "abs"
                                                                                                                                            }
                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                            [ Expression'BitwiseOr
                                                                                                                                                ( Expression'Minus
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
                                                                                                                                                ( Expression'Plus
                                                                                                                                                    ( Expression'Minus
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
                                                                                                                                                        ( Expression'Negate
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
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Multiply
                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                            ( Number "19" )
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
                                                                                                                        )
                                                                                                                    ]
                                                                                                                }
                                                                                                            , filter = Nothing
                                                                                                            , over = Nothing
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                )
                                                                                                ( Expression'Negate
                                                                                                    ( Expression'Case
                                                                                                        ( CaseExpression
                                                                                                            { base = Just
                                                                                                                ( Expression'FunctionCall
                                                                                                                    ( FunctionCallExpression
                                                                                                                        { call = FunctionCall
                                                                                                                            { name = Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "min"
                                                                                                                                }
                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                [ Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "a"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                ]
                                                                                                                            }
                                                                                                                        , filter = Nothing
                                                                                                                        , over = Nothing
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            , cases =
                                                                                                                ( Expression'AggregateDistinctFunctionCall
                                                                                                                    ( AggregateDistinctFunctionCallExpression
                                                                                                                        { call = FunctionCall
                                                                                                                            { name = Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "count"
                                                                                                                                }
                                                                                                                            , arguments = Identity
                                                                                                                                ( Expression'Negate
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "a"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            }
                                                                                                                        , filter = Nothing
                                                                                                                        }
                                                                                                                    )
                                                                                                                , Expression'AggregateDistinctFunctionCall
                                                                                                                    ( AggregateDistinctFunctionCallExpression
                                                                                                                        { call = FunctionCall
                                                                                                                            { name = Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "count"
                                                                                                                                }
                                                                                                                            , arguments = Identity
                                                                                                                                ( Expression'LiteralValue
                                                                                                                                    ( Number "19" )
                                                                                                                                )
                                                                                                                            }
                                                                                                                        , filter = Nothing
                                                                                                                        }
                                                                                                                    )
                                                                                                                ) :| []
                                                                                                            , else_ = Expression'FunctionCall
                                                                                                                ( FunctionCallExpression
                                                                                                                    { call = FunctionCall
                                                                                                                        { name = Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "count"
                                                                                                                            }
                                                                                                                        , arguments = FunctionArguments'Wildcard
                                                                                                                        }
                                                                                                                    , filter = Nothing
                                                                                                                    , over = Nothing
                                                                                                                    }
                                                                                                                )
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                )
                                                                                            )
                                                                                            ( Expression'Multiply
                                                                                                ( Expression'AggregateDistinctFunctionCall
                                                                                                    ( AggregateDistinctFunctionCallExpression
                                                                                                        { call = FunctionCall
                                                                                                            { name = Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "count"
                                                                                                                }
                                                                                                            , arguments = Identity
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
                                                                                                            }
                                                                                                        , filter = Nothing
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                                ( Expression'FunctionCall
                                                                                                    ( FunctionCallExpression
                                                                                                        { call = FunctionCall
                                                                                                            { name = Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "max"
                                                                                                                }
                                                                                                            , arguments = FunctionArguments'Arguments
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
                                                                                                                ]
                                                                                                            }
                                                                                                        , filter = Nothing
                                                                                                        , over = Nothing
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                        ( Expression'AggregateDistinctFunctionCall
                                                                                            ( AggregateDistinctFunctionCallExpression
                                                                                                { call = FunctionCall
                                                                                                    { name = Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "count"
                                                                                                        }
                                                                                                    , arguments = Identity
                                                                                                        ( Expression'LiteralValue
                                                                                                            ( Number "19" )
                                                                                                        )
                                                                                                    }
                                                                                                , filter = Nothing
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
                                                        )
                                                    , cases =
                                                        ( Expression'Subquery
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
                                                                                                    , value = "count"
                                                                                                    }
                                                                                                , arguments = FunctionArguments'Wildcard
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
                                                            , value = "d"
                                                            }
                                                        )
                                                    }
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "11" )
                                            )
                                        )
                                    )
                                )
                                ( Expression'Multiply
                                    ( Expression'LiteralValue
                                        ( Number "11" )
                                    )
                                    ( Expression'Negate
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "c"
                                                }
                                            )
                                        )
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
                            ( Expression'Between
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "f"
                                        }
                                    )
                                )
                                ( Expression'BitwiseOr
                                    ( Expression'Case
                                        ( CaseExpression
                                            { base = Just
                                                ( Expression'Case
                                                    ( CaseExpression
                                                        { base = Nothing
                                                        , cases =
                                                            ( Expression'InSubquery
                                                                ( InSubqueryExpression
                                                                    { expression = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
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
                                                                                                        { namespace = Just
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "t1"
                                                                                                                }
                                                                                                            )
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
                                                                            )
                                                                            ( SelectCore'Select
                                                                                ( Select
                                                                                    { distinct = False
                                                                                    , columns = ResultColumn'Expression
                                                                                        ( Aliased
                                                                                            { value = Expression'Case
                                                                                                ( CaseExpression
                                                                                                    { base = Nothing
                                                                                                    , cases =
                                                                                                        ( Expression'Or
                                                                                                            ( Expression'And
                                                                                                                ( Expression'InValues
                                                                                                                    ( InValuesExpression
                                                                                                                        { expression = Expression'Column
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
                                                                                                                        , values =
                                                                                                                            [ Expression'Divide
                                                                                                                                ( Expression'FunctionCall
                                                                                                                                    ( FunctionCallExpression
                                                                                                                                        { call = FunctionCall
                                                                                                                                            { name = Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "abs"
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
                                                                                                                                )
                                                                                                                                ( Expression'FunctionCall
                                                                                                                                    ( FunctionCallExpression
                                                                                                                                        { call = FunctionCall
                                                                                                                                            { name = Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "abs"
                                                                                                                                                }
                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                [ Expression'LiteralValue
                                                                                                                                                    ( Number "11" )
                                                                                                                                                ]
                                                                                                                                            }
                                                                                                                                        , filter = Nothing
                                                                                                                                        , over = Nothing
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            , Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "f"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "c"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            ]
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                                ( Expression'InSubquery
                                                                                                                    ( InSubqueryExpression
                                                                                                                        { expression = Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "d"
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
                                                                                                                                                    { value = Expression'Multiply
                                                                                                                                                        ( Expression'Negate
                                                                                                                                                            ( Expression'AggregateDistinctFunctionCall
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
                                                                                                                                                                                    , value = "b"
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            )
                                                                                                                                                                        }
                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                        ( Expression'FunctionCall
                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "count"
                                                                                                                                                                        }
                                                                                                                                                                    , arguments = FunctionArguments'Wildcard
                                                                                                                                                                    }
                                                                                                                                                                , filter = Nothing
                                                                                                                                                                , over = Nothing
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
                                                                                                                                                { value = Expression'AggregateDistinctFunctionCall
                                                                                                                                                    ( AggregateDistinctFunctionCallExpression
                                                                                                                                                        { call = FunctionCall
                                                                                                                                                            { name = Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "count"
                                                                                                                                                                }
                                                                                                                                                            , arguments = Identity
                                                                                                                                                                ( Expression'Negate
                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                            }
                                                                                                                                                        , filter = Nothing
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
                                                                                                            ( Expression'GreaterThan
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
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "19" )
                                                                                                                )
                                                                                                            )
                                                                                                        , Expression'Case
                                                                                                            ( CaseExpression
                                                                                                                { base = Nothing
                                                                                                                , cases =
                                                                                                                    ( Expression'InValues
                                                                                                                        ( InValuesExpression
                                                                                                                            { expression = Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "f"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , values =
                                                                                                                                [ Expression'LiteralValue
                                                                                                                                    ( Number "11" )
                                                                                                                                , Expression'LiteralValue
                                                                                                                                    ( Number "11" )
                                                                                                                                , Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "d"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                ]
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
                                                                                                                            , value = "e"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    ) :|
                                                                                                                    [
                                                                                                                        ( Expression'LessThanOrEquals
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
                                                                                                                            ( Expression'LiteralValue
                                                                                                                                ( Number "19" )
                                                                                                                            )
                                                                                                                        , Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "a"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    ]
                                                                                                                , else_ = Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "e"
                                                                                                                        }
                                                                                                                    )
                                                                                                                }
                                                                                                            )
                                                                                                        ) :| []
                                                                                                    , else_ = Expression'Negate
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
                                                                    , value = "e"
                                                                    }
                                                                )
                                                            ) :|
                                                            [
                                                                ( Expression'Equals
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
                                                                            , value = "b"
                                                                            }
                                                                        )
                                                                    )
                                                                , Expression'LiteralValue
                                                                    ( Number "19" )
                                                                )
                                                            ]
                                                        , else_ = Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "e"
                                                                }
                                                            )
                                                        }
                                                    )
                                                )
                                            , cases =
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "f"
                                                        }
                                                    )
                                                , Expression'LiteralValue
                                                    ( Number "19" )
                                                ) :| []
                                            , else_ = Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "c"
                                                    }
                                                )
                                            }
                                        )
                                    )
                                    ( Expression'Minus
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "d"
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
                                )
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "a"
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