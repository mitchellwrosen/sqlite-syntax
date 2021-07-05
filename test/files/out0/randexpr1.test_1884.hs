Statement'Select
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
                                    { base = Just
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
                                                                                            , value = "a"
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
                                                                                                                ( Expression'Minus
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
                                                                                                                            , value = "b"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                )
                                                                                                                ( Expression'BitwiseOr
                                                                                                                    ( Expression'Minus
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
                                                                ( Expression'Case
                                                                    ( CaseExpression
                                                                        { base = Nothing
                                                                        , cases =
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
                                                                                            , value = "d"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Plus
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "17" )
                                                                                        )
                                                                                        ( Expression'Case
                                                                                            ( CaseExpression
                                                                                                { base = Nothing
                                                                                                , cases =
                                                                                                    ( Expression'InValues
                                                                                                        ( InValuesExpression
                                                                                                            { expression = Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
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
                                                                                                                , Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "a"
                                                                                                                        }
                                                                                                                    )
                                                                                                                , Expression'Multiply
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
                                                                                                                                            [ Expression'BitwiseNegate
                                                                                                                                                ( Expression'Case
                                                                                                                                                    ( CaseExpression
                                                                                                                                                        { base = Nothing
                                                                                                                                                        , cases =
                                                                                                                                                            ( Expression'LessThanOrEquals
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
                                                                                                                                                                        , value = "c"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                            , Expression'Negate
                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                    ( Number "17" )
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
                                                                                                                                                ( Number "19" )
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
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "d"
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
                                                                                                                ]
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
                                                                                                        { namespace = Nothing
                                                                                                        , value = "a"
                                                                                                        }
                                                                                                    )
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                )
                                                                            , Expression'LiteralValue
                                                                                ( Number "11" )
                                                                            ) :| []
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
                                        )
                                    , cases =
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "f"
                                                }
                                            )
                                        , Expression'LiteralValue
                                            ( Number "17" )
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
                        ( Expression'Or
                            ( Expression'And
                                ( Expression'LessThan
                                    ( Expression'Minus
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
                                                    , value = "e"
                                                    }
                                                )
                                            )
                                            ( Expression'Case
                                                ( CaseExpression
                                                    { base = Just
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
                                                            ( Expression'Case
                                                                ( CaseExpression
                                                                    { base = Just
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "c"
                                                                                }
                                                                            )
                                                                        )
                                                                    , cases =
                                                                        ( Expression'Divide
                                                                            ( Expression'FunctionCall
                                                                                ( FunctionCallExpression
                                                                                    { call = FunctionCall
                                                                                        { name = Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "abs"
                                                                                            }
                                                                                        , arguments = FunctionArguments'Arguments
                                                                                            [ Expression'Multiply
                                                                                                ( Expression'BitwiseNegate
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
                                                                                                                                            , value = "min"
                                                                                                                                            }
                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                            [ Expression'Minus
                                                                                                                                                ( Expression'Multiply
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
                                                                                                                                                                                                ( Expression'Plus
                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "d"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    )
                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                        ( Number "19" )
                                                                                                                                                                                                    )
                                                                                                                                                                                                )
                                                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                                                    ( Number "11" )
                                                                                                                                                                                                )
                                                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                                                    ( Number "19" )
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
                                                                                                                                                                , Expression'Plus
                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                        ( Number "11" )
                                                                                                                                                                    )
                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                    )
                                                                                                                                                                ) :|
                                                                                                                                                                [
                                                                                                                                                                    ( Expression'And
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
                                                                                                                                                                                        , Expression'LiteralValue
                                                                                                                                                                                            ( Number "17" )
                                                                                                                                                                                        , Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "e"
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
                                                                                                                                                                                    , value = "c"
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
                                                                                                                                                                    )
                                                                                                                                                                ]
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
                                                                                                                                                            , value = "a"
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
                                                                        , Expression'LiteralValue
                                                                            ( Number "13" )
                                                                        ) :| []
                                                                    , else_ = Expression'LiteralValue
                                                                        ( Number "17" )
                                                                    }
                                                                )
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
                                                                , value = "f"
                                                                }
                                                            )
                                                        , Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "f"
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
                                ( Expression'Between
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
                                            , value = "a"
                                            }
                                        )
                                    )
                                )
                            )
                            ( Expression'LessThan
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
                                        , value = "f"
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