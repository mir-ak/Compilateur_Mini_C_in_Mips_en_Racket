.data
nl: .asciiz "\n"

.text
pair:
	addi $sp, $sp, -12
	sw $ra, 0($sp)
	sw $fp, 4($sp)
	addi $fp, $sp, 12
	li $a0, 8
	li $v0, 9
	syscall
	sw $v0, 8($sp)
	lw $v0, 4($fp)
	move $t0, $v0
	lw $v0, 8($sp)
	sw $t0, 0($v0)
	lw $v0, 0($fp)
	move $t0, $v0
	lw $v0, 8($sp)
	sw $t0, 4($v0)
	lw $v0, 8($sp)
	lw $ra, 0($sp)
	lw $fp, 4($sp)
	addi $sp, $sp, 12
	jr $ra
head:
	addi $sp, $sp, -8
	sw $ra, 0($sp)
	sw $fp, 4($sp)
	addi $fp, $sp, 8
	lw $v0, 0($fp)
	lw $v0, 0($v0)
	lw $ra, 0($sp)
	lw $fp, 4($sp)
	addi $sp, $sp, 8
	jr $ra
tail:
	addi $sp, $sp, -8
	sw $ra, 0($sp)
	sw $fp, 4($sp)
	addi $fp, $sp, 8
	lw $v0, 0($fp)
	lw $v0, 4($v0)
	lw $ra, 0($sp)
	lw $fp, 4($sp)
	addi $sp, $sp, 8
	jr $ra
main:
	addi $sp, $sp, -8
	sw $ra, 0($sp)
	sw $fp, 4($sp)
	addi $fp, $sp, 8
	li $v0, 1
	addi $sp, $sp, -4
	sw $v0, 0($sp)
	lw $v0, -12($fp)
	addi $sp, $sp, -4
	sw $v0, 0($sp)
	li $v0, 5
	addi $sp, $sp, -4
	sw $v0, 0($sp)
	lw $t0, 4($sp)
	lw $t1, 0($sp)
	add $v0, $t0, $t1
	addi $sp, $sp, 8
	addi $sp, $sp, -4
	sw $v0, 0($sp)
	li $v0, 45
	addi $sp, $sp, -4
	sw $v0, 0($sp)
	lw $t0, 4($sp)
	lw $t1, 0($sp)
	add $v0, $t0, $t1
	addi $sp, $sp, 8
	addi $sp, $sp, -4
	sw $v0, 0($sp)
	lw $v0, -16($fp)
	addi $sp, $sp, -4
	sw $v0, 0($sp)
	lw $a0, 0($sp)
	li $v0, 1
	syscall
	addi $sp, $sp, 4
	la $a0, nl
	li $v0, 4
	syscall
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $ra, 0($sp)
	lw $fp, 4($sp)
	addi $sp, $sp, 8
	jr $ra
