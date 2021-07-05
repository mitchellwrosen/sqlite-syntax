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
                                ( Expression'Plus
                                    ( Expression'Case
                                        ( CaseExpression
                                            { base = Nothing
                                            , cases =
                                                ( Expression'LessThan
                                                    ( Expression'Minus
                                                        ( Expression'Minus
                                                            ( Expression'Case
                                                                ( CaseExpression
                                                                    { base = Nothing
                                                                    , cases =
                                                                        ( Expression'Between
                                                                            ( Expression'LiteralValue
                                                                                ( Number "11" )
                                                                            )
                                                                            ( Expression'Plus
                                                                                ( Expression'Plus
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "a"
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
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "b"
                                                                                    }
                                                                                )
                                                                            )
                                                                        , Expression'Minus
                                                                            ( Expression'Multiply
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
                                                                            ( Expression'Multiply
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
                                                                        ) :|
                                                                        [
                                                                            ( Expression'LessThanOrEquals
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "b"
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( Expression'Plus
                                                                                    ( Expression'Minus
                                                                                        ( Expression'Minus
                                                                                            ( Expression'Negate
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
                                                                                    ( Expression'Multiply
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
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
                                                                                    )
                                                                                )
                                                                            , Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "e"
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
                                                            )
                                                            ( Expression'LiteralValue
                                                                ( Number "11" )
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
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "d"
                                                            }
                                                        )
                                                    )
                                                , Expression'LiteralValue
                                                    ( Number "11" )
                                                ) :|
                                                [
                                                    ( Expression'Equals
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
                                                                , value = "e"
                                                                }
                                                            )
                                                        )
                                                    , Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "a"
                                                            }
                                                        )
                                                    )
                                                ]
                                            , else_ = Expression'LiteralValue
                                                ( Number "13" )
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
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "d"
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
                                    { expression = Expression'BitwiseNegate
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
                                                                                                    [ Expression'Plus
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
                                                                                ( Expression'InValues
                                                                                    ( InValuesExpression
                                                                                        { expression = Expression'LiteralValue
                                                                                            ( Number "17" )
                                                                                        , values =
                                                                                            [ Expression'BitwiseNegate
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
                                                                                                    { namespace = Nothing
                                                                                                    , value = "d"
                                                                                                    }
                                                                                                )
                                                                                            , Expression'LiteralValue
                                                                                                ( Number "13" )
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
                                                        , Expression'BitwiseOr
                                                            ( Expression'Negate
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
                                                                                                            , value = "max"
                                                                                                            }
                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                            [ Expression'LiteralValue
                                                                                                                ( Number "13" )
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
                                                                                                    ( Expression'InValues
                                                                                                        ( InValuesExpression
                                                                                                            { expression = Expression'Multiply
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
                                                                                                                    ( Expression'Negate
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
                                                                                                                                                                    { value = Expression'LiteralValue
                                                                                                                                                                        ( Number "17" )
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
                                                                                                                                                                            { expression = Expression'BitwiseOr
                                                                                                                                                                                ( Expression'BitwiseOr
                                                                                                                                                                                    ( Expression'Multiply
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
                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                        ( Number "17" )
                                                                                                                                                                                    )
                                                                                                                                                                                )
                                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                                    ( Number "11" )
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
                                                                                                                                            ( Number "13" )
                                                                                                                                        ]
                                                                                                                                    }
                                                                                                                                , filter = Nothing
                                                                                                                                , over = Nothing
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    )
                                                                                                                )
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "13" )
                                                                                                                )
                                                                                                            , values =
                                                                                                                [ Expression'LiteralValue
                                                                                                                    ( Number "11" )
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
                                    , values =
                                        [ Expression'LiteralValue
                                            ( Number "13" )
                                        , Expression'LiteralValue
                                            ( Number "13" )
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