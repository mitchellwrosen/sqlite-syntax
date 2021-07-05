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
                                                                        ( Expression'Not
                                                                            ( Expression'Between
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
                                                                                                        [ Expression'BitwiseOr
                                                                                                            ( Expression'Plus
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
                                                                                                                                                                            [ Expression'BitwiseOr
                                                                                                                                                                                ( Expression'BitwiseOr
                                                                                                                                                                                    ( Expression'Plus
                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "b"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                        ( Expression'Negate
                                                                                                                                                                                            ( Expression'Case
                                                                                                                                                                                                ( CaseExpression
                                                                                                                                                                                                    { base = Nothing
                                                                                                                                                                                                    , cases =
                                                                                                                                                                                                        ( Expression'Equals
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
                                                                                                                                                                                                        ) :|
                                                                                                                                                                                                        [
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
                                                                                                                                                                                                                )
                                                                                                                                                                                                            , Expression'Column
                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "d"
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
                                                                                                                                                                                                , value = "e"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                    )
                                                                                                                                                                                )
                                                                                                                                                                                ( Expression'Multiply
                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                                    )
                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                            , value = "e"
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
                                                                                                                                                                                                    , value = "e"
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
                                                                                                            )
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
                                            )
                                        , Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "e"
                                                }
                                            )
                                        ) :| []
                                    , else_ = Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
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
                        ( Expression'Not
                            ( Expression'Not
                                ( Expression'Between
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
                                                    { namespace = Nothing
                                                    , value = "d"
                                                    }
                                                )
                                            )
                                        )
                                        ( Expression'Case
                                            ( CaseExpression
                                                { base = Just
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "b"
                                                            }
                                                        )
                                                    )
                                                , cases =
                                                    ( Expression'Plus
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
                                                    , Expression'Plus
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
                                                        ( Expression'Negate
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
                                                    ) :| []
                                                , else_ = Expression'Plus
                                                    ( Expression'LiteralValue
                                                        ( Number "11" )
                                                    )
                                                    ( Expression'Multiply
                                                        ( Expression'Multiply
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
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
                                                                                                        ( Expression'LiteralValue
                                                                                                            ( Number "11" )
                                                                                                        )
                                                                                                        ( Expression'Plus
                                                                                                            ( Expression'Negate
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
                                                        )
                                                        ( Expression'LiteralValue
                                                            ( Number "19" )
                                                        )
                                                    )
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
                                                    , value = "c"
                                                    }
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "11" )
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "17" )
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