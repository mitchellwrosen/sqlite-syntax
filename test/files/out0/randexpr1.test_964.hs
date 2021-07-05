Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Multiply
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
                                                                                                    , value = "e"
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
                                                                        ( Expression'LessThanOrEquals
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
                                                                            ( Expression'Case
                                                                                ( CaseExpression
                                                                                    { base = Just
                                                                                        ( Expression'Minus
                                                                                            ( Expression'Case
                                                                                                ( CaseExpression
                                                                                                    { base = Nothing
                                                                                                    , cases =
                                                                                                        ( Expression'Between
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
                                                                                                                                                        { value = Expression'Minus
                                                                                                                                                            ( Expression'Minus
                                                                                                                                                                ( Expression'Divide
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
                                                                                                                                                                ( Expression'Divide
                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "abs"
                                                                                                                                                                                    }
                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                    [ Expression'Negate
                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "e"
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
                                                                                                                                                                                    [ Expression'Plus
                                                                                                                                                                                        ( Expression'Multiply
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
                                                                                                                                                                                                        , value = "b"
                                                                                                                                                                                                        }
                                                                                                                                                                                                    )
                                                                                                                                                                                                )
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                        ( Expression'Multiply
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
                                                                                                                                                                                    ]
                                                                                                                                                                                }
                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                            , over = Nothing
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
                                                                                                                            , Expression'Negate
                                                                                                                                ( Expression'LiteralValue
                                                                                                                                    ( Number "11" )
                                                                                                                                )
                                                                                                                            ]
                                                                                                                        }
                                                                                                                    , filter = Nothing
                                                                                                                    , over = Nothing
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'LiteralValue
                                                                                                                ( Number "17" )
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
                                                                                                        ) :|
                                                                                                        [
                                                                                                            ( Expression'GreaterThan
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
                                                                                                            , Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "b"
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        ]
                                                                                                    , else_ = Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "b"
                                                                                                            }
                                                                                                        )
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
                                                                                    , cases =
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "d"
                                                                                                }
                                                                                            )
                                                                                        , Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "d"
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
                        ( Expression'Not
                            ( Expression'InValues
                                ( InValuesExpression
                                    { expression = Expression'Multiply
                                        ( Expression'Multiply
                                            ( Expression'BitwiseNegate
                                                ( Expression'Negate
                                                    ( Expression'Case
                                                        ( CaseExpression
                                                            { base = Nothing
                                                            , cases =
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
                                                                                            ( Expression'Between
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
                                                                                                ( Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "b"
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                                ( Expression'BitwiseOr
                                                                                                    ( Expression'LiteralValue
                                                                                                        ( Number "11" )
                                                                                                    )
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "d"
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
                                                                    )
                                                                , Expression'Negate
                                                                    ( Expression'Divide
                                                                        ( Expression'FunctionCall
                                                                            ( FunctionCallExpression
                                                                                { call = FunctionCall
                                                                                    { name = Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "abs"
                                                                                        }
                                                                                    , arguments = FunctionArguments'Arguments
                                                                                        [ Expression'Plus
                                                                                            ( Expression'LiteralValue
                                                                                                ( Number "17" )
                                                                                            )
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "d"
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
                                                                ) :| []
                                                            , else_ = Expression'Case
                                                                ( CaseExpression
                                                                    { base = Nothing
                                                                    , cases =
                                                                        ( Expression'Not
                                                                            ( Expression'GreaterThanOrEquals
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
                                                                                ( Expression'Case
                                                                                    ( CaseExpression
                                                                                        { base = Nothing
                                                                                        , cases =
                                                                                            ( Expression'InValues
                                                                                                ( InValuesExpression
                                                                                                    { expression = Expression'Case
                                                                                                        ( CaseExpression
                                                                                                            { base = Just
                                                                                                                ( Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "a"
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            , cases =
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
                                                                                                                                    , value = "e"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , Expression'LiteralValue
                                                                                                                                ( Number "17" )
                                                                                                                            ) :| []
                                                                                                                        , else_ = Expression'LiteralValue
                                                                                                                            ( Number "19" )
                                                                                                                        }
                                                                                                                    )
                                                                                                                , Expression'LiteralValue
                                                                                                                    ( Number "13" )
                                                                                                                ) :| []
                                                                                                            , else_ = Expression'LiteralValue
                                                                                                                ( Number "19" )
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
                                                                                                                , value = "b"
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
                                                                                                                , value = "b"
                                                                                                                }
                                                                                                            )
                                                                                                        ]
                                                                                                    }
                                                                                                )
                                                                                            , Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "d"
                                                                                                    }
                                                                                                )
                                                                                            ) :|
                                                                                            [
                                                                                                ( Expression'Not
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
                                                                                                                    , value = "d"
                                                                                                                    }
                                                                                                                )
                                                                                                            , values =
                                                                                                                [ Expression'LiteralValue
                                                                                                                    ( Number "19" )
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
                                                                                                                , Expression'Negate
                                                                                                                    ( Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "c"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                ]
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                , Expression'LiteralValue
                                                                                                    ( Number "11" )
                                                                                                )
                                                                                            ]
                                                                                        , else_ = Expression'Negate
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "a"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        }
                                                                                    )
                                                                                )
                                                                            )
                                                                        , Expression'LiteralValue
                                                                            ( Number "11" )
                                                                        ) :| []
                                                                    , else_ = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "c"
                                                                            }
                                                                        )
                                                                    }
                                                                )
                                                            }
                                                        )
                                                    )
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "17" )
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
                                    , values =
                                        [ Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "a"
                                                }
                                            )
                                        , Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "b"
                                                }
                                            )
                                        , Expression'LiteralValue
                                            ( Number "13" )
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