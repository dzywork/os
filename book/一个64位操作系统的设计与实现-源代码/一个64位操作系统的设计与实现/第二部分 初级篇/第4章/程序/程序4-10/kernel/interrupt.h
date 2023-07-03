/***************************************************
*		版权声明
*
*	本操作系统名为：MINE
*	该操作系统未经授权不得以盈利或非盈利为目的进行开发，
*	只允许个人学习以及公开交流使用
*
*	代码最终所有权及解释权归田宇所有；
*
*	本模块作者：	田宇
*	EMail:		345538255@qq.com
*
*
***************************************************/
#ifndef __INTERRUPT_H__
#define __INTERRUPT_H__
#include "linkage.h"

/*

*/

void init_interrupt();

/*

*/

void do_IRQ(unsigned long regs,unsigned long nr);

#endif
