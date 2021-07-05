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
                                                                                            , value = "d"
                                                                                            }
                                                                                        )
                                                                                    , Expression'LiteralValue
                                                                                        ( Number "17" )
                                                                                    , Expression'BitwiseOr
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
                                                                                                                            , where_ = Just
                                                                                                                                ( Expression'And
                                                                                                                                    ( Expression'Not
                                                                                                                                        ( Expression'InValues
                                                                                                                                            ( InValuesExpression
                                                                                                                                                { expression = Expression'Minus
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
                                                                                                                                                , values =
                                                                                                                                                    [ Expression'Plus
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
                                                                                                                                                            ( Expression'Case
                                                                                                                                                                ( CaseExpression
                                                                                                                                                                    { base = Nothing
                                                                                                                                                                    , cases =
                                                                                                                                                                        ( Expression'Not
                                                                                                                                                                            ( Expression'Between
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
                                                                                                                                                                                        , value = "c"
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
                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                , value = "a"
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
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                            ( Number "17" )
                                                                                                                                                        )
                                                                                                                                                    , Expression'LiteralValue
                                                                                                                                                        ( Number "13" )
                                                                                                                                                    , Expression'Negate
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
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                    ( Expression'Not
                                                                                                                                        ( Expression'Between
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
                                                                                                        ]
                                                                                                    }
                                                                                                , filter = Nothing
                                                                                                , over = Nothing
                                                                                                }
                                                                                            )
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
                                        ) :|
                                        [
                                            ( Expression'Or
                                                ( Expression'LessThan
                                                    ( Expression'LiteralValue
                                                        ( Number "17" )
                                                    )
                                                    ( Expression'LiteralValue
                                                        ( Number "17" )
                                                    )
                                                )
                                                ( Expression'NotEquals
                                                    ( Expression'LiteralValue
                                                        ( Number "13" )
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
                                                    { namespace = Nothing
                                                    , value = "f"
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
                                            , value = "f"
                                            }
                                        )
                                    , values =
                                        [ Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "e"
                                                }
                                            )
                                        , Expression'LiteralValue
                                            ( Number "19" )
                                        , Expression'Case
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
                                                                                ( Expression'LessThan
                                                                                    ( Expression'Plus
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
                                                                                                                                                                                [ Expression'Case
                                                                                                                                                                                    ( CaseExpression
                                                                                                                                                                                        { base = Just
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
                                                                                                                                                                                        , cases =
                                                                                                                                                                                            ( Expression'Plus
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
                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                        ( Number "13" )
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
                                                                                                                                                                                                                                        ( Expression'GreaterThan
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
                                                                                                                                                                                            , Expression'Column
                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                    , value = "f"
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            ) :| []
                                                                                                                                                                                        , else_ = Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "b"
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
                                                    , Expression'Multiply
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
                                                    ) :| []
                                                , else_ = Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "d"
                                                        }
                                                    )
                                                }
                                            )
                                        ]
                                    }
                                )
                            )
                            ( Expression'Not
                                ( Expression'InValues
                                    ( InValuesExpression
                                        { expression = Expression'LiteralValue
                                            ( Number "17" )
                                        , values =
                                            [ Expression'LiteralValue
                                                ( Number "17" )
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
                                            , Expression'LiteralValue
                                                ( Number "19" )
                                            ]
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