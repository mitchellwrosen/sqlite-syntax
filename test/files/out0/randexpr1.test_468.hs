Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Plus
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "d"
                                        }
                                    )
                                )
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Nothing
                                        , cases =
                                            ( Expression'Equals
                                                ( Expression'Minus
                                                    ( Expression'Minus
                                                        ( Expression'Minus
                                                            ( Expression'Minus
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
                                                                                                                            { namespace = Just
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "t1"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , value = "a"
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
                                                                                                                                , value = "d"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , Expression'Plus
                                                                                                                            ( Expression'Multiply
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "f"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                )
                                                                                                                                ( Expression'Subquery
                                                                                                                                    ( SelectStatement
                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                        , select = CompoundSelect
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
                                                                                , Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "b"
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
                                                                        , value = "c"
                                                                        }
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
                                                                , value = "c"
                                                                }
                                                            )
                                                        )
                                                    )
                                                    ( Expression'Case
                                                        ( CaseExpression
                                                            { base = Nothing
                                                            , cases =
                                                                ( Expression'NotEquals
                                                                    ( Expression'Minus
                                                                        ( Expression'Multiply
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
                                                                                    , value = "a"
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
                                                                                    { namespace = Nothing
                                                                                    , value = "e"
                                                                                    }
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Expression'LiteralValue
                                                                        ( Number "11" )
                                                                    )
                                                                , Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "f"
                                                                        }
                                                                    )
                                                                ) :|
                                                                [
                                                                    ( Expression'Not
                                                                        ( Expression'Between
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
                                                                            ( Expression'LiteralValue
                                                                                ( Number "11" )
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
                                                                    , value = "f"
                                                                    }
                                                                )
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
                                                        , value = "d"
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
                                                    , value = "f"
                                                    }
                                                )
                                            ) :|
                                            [
                                                ( Expression'Not
                                                    ( Expression'GreaterThan
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
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
                                                                , value = "f"
                                                                }
                                                            )
                                                        )
                                                    )
                                                , Expression'LiteralValue
                                                    ( Number "17" )
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
                                                        ( Expression'GreaterThanOrEquals
                                                            ( Expression'Multiply
                                                                ( Expression'LiteralValue
                                                                    ( Number "11" )
                                                                )
                                                                ( Expression'Negate
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "d"
                                                                            }
                                                                        )
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
                                ( Expression'LessThan
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
                                    ( Expression'Multiply
                                        ( Expression'Negate
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "e"
                                                    }
                                                )
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "11" )
                                        )
                                    )
                                )
                            )
                            ( Expression'Not
                                ( Expression'Between
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
                                            , value = "d"
                                            }
                                        )
                                    )
                                    ( Expression'BitwiseOr
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
                                                            , value = "a"
                                                            }
                                                        )
                                                    )
                                                , cases =
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
                                                    , Expression'Minus
                                                        ( Expression'Plus
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
                                                                    , value = "f"
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
                                                    ) :| []
                                                , else_ = Expression'LiteralValue
                                                    ( Number "11" )
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