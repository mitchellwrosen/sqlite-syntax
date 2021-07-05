Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Divide
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "abs"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Minus
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
                                                    ( Expression'Case
                                                        ( CaseExpression
                                                            { base = Nothing
                                                            , cases =
                                                                ( Expression'LessThanOrEquals
                                                                    ( Expression'Plus
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
                                                                                                                                    [ Expression'Multiply
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
                                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                , value = "a"
                                                                                                                                                                                                                }
                                                                                                                                                                                                            )
                                                                                                                                                                                                        )
                                                                                                                                                                                                        ( Expression'Multiply
                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
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
                                                                                                                                                                                                                    , value = "f"
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
                                                                                                                                                                                    ( Expression'InValues
                                                                                                                                                                                        ( InValuesExpression
                                                                                                                                                                                            { expression = Expression'Plus
                                                                                                                                                                                                ( Expression'Minus
                                                                                                                                                                                                    ( Expression'Multiply
                                                                                                                                                                                                        ( Expression'BitwiseNegate
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
                                                                                                                                                                                                ( Expression'Multiply
                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "f"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    )
                                                                                                                                                                                                    ( Expression'Negate
                                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                                            ( Number "19" )
                                                                                                                                                                                                        )
                                                                                                                                                                                                    )
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
                                                                                                                                                                                                        , value = "d"
                                                                                                                                                                                                        }
                                                                                                                                                                                                    )
                                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                        , value = "e"
                                                                                                                                                                                                        }
                                                                                                                                                                                                    )
                                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                        , value = "b"
                                                                                                                                                                                                        }
                                                                                                                                                                                                    )
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
                                                                                                                                                        , Expression'LiteralValue
                                                                                                                                                            ( Number "11" )
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
                                                                                                                                                , value = "a"
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
                                                                                                                    ( Expression'Or
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
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "b"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        , Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "c"
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
                                                                                                                        )
                                                                                                                        ( Expression'Between
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
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "b"
                                                                                                                                        }
                                                                                                                                    )
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
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "a"
                                                                                }
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "e"
                                                                            }
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
                                                                        , value = "d"
                                                                        }
                                                                    )
                                                                ) :| []
                                                            , else_ = Expression'LiteralValue
                                                                ( Number "17" )
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
                        ( Expression'NotEquals
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
                                            , value = "d"
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
                                ( Expression'BitwiseNegate
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
                            ( Expression'Case
                                ( CaseExpression
                                    { base = Nothing
                                    , cases =
                                        ( Expression'InSubquery
                                            ( InSubqueryExpression
                                                { expression = Expression'LiteralValue
                                                    ( Number "17" )
                                                , subquery = SelectStatement
                                                    { commonTableExpressions = Nothing
                                                    , select = Union
                                                        ( CompoundSelect
                                                            ( SelectCore'Select
                                                                ( Select
                                                                    { distinct = False
                                                                    , columns = ResultColumn'Expression
                                                                        ( Aliased
                                                                            { value = Expression'Negate
                                                                                ( Expression'FunctionCall
                                                                                    ( FunctionCallExpression
                                                                                        { call = FunctionCall
                                                                                            { name = Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "abs"
                                                                                                }
                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                [ Expression'Minus
                                                                                                    ( Expression'AggregateDistinctFunctionCall
                                                                                                        ( AggregateDistinctFunctionCallExpression
                                                                                                            { call = FunctionCall
                                                                                                                { name = Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "count"
                                                                                                                    }
                                                                                                                , arguments = Identity
                                                                                                                    ( Expression'Plus
                                                                                                                        ( Expression'Multiply
                                                                                                                            ( Expression'Multiply
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
                                                                                                                                    ( Number "17" )
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
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "f"
                                                                                                                                }
                                                                                                                            )
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
                                                                                                                    [ Expression'LiteralValue
                                                                                                                        ( Number "19" )
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
                                                                        { value = Expression'Minus
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
                                                                            ( Expression'FunctionCall
                                                                                ( FunctionCallExpression
                                                                                    { call = FunctionCall
                                                                                        { name = Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "max"
                                                                                            }
                                                                                        , arguments = FunctionArguments'Arguments
                                                                                            [ Expression'Multiply
                                                                                                ( Expression'Case
                                                                                                    ( CaseExpression
                                                                                                        { base = Just
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
                                                                                                        , cases =
                                                                                                            ( Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "a"
                                                                                                                    }
                                                                                                                )
                                                                                                            , Expression'Plus
                                                                                                                ( Expression'Plus
                                                                                                                    ( Expression'LiteralValue
                                                                                                                        ( Number "11" )
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
                                                                                                                                , value = "b"
                                                                                                                                }
                                                                                                                            )
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
                                                                                                                        , value = "f"
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            ) :| []
                                                                                                        , else_ = Expression'LiteralValue
                                                                                                            ( Number "19" )
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
                                                                                            ]
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
                                                    , orderBy = Nothing
                                                    , limit = Nothing
                                                    }
                                                }
                                            )
                                        , Expression'Divide
                                            ( Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "abs"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'Minus
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "b"
                                                                        }
                                                                    )
                                                                )
                                                                ( Expression'Negate
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "f"
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
                                        ) :| []
                                    , else_ = Expression'Column
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