#!/usr/bin/python

"""
	Name		: uef2wave
	Author		: Wouter Hobers
	Created		: May 21 2001
	Last edited	: May 23 2001
	Purpose		: Convert tape blocks of UEF files to WAVE files so they can be
			  recorded on tape and used with a real Electron.
	WWW		: http://www.fmf.nl/~xaviar/acorn/
	Email		: xaviar@fmf.nl

	Modified        : 2017 Gary Preston
	Minor tweak to export the raw bitstream rather than waveform to disk for
	use with Acorn core's raw tape loader on the FPGA Replay.
"""


import sys, math, gzip
import struct

if sys.platform == "mac":
	import macfs


global TRUE
global FALSE
global BAUD

TRUE  = 1 == 1
FALSE = not TRUE

BAUD = 1000000.0/(16.0*52.0)


class UEFParser:

	
	tapeID     = 0x0100
	highToneID = 0x0110
	gapID      = 0x0112
	
	tapeIDList = [highToneID, gapID, tapeID]

	startBit = "0"
	stopBit  = "1"

	buffer = []
	pos    = 0


	def __init__(self,buffer):

		self.buffer = buffer


	def WordAt(self,buffer,pos):
		
		if pos + 1 >= len(buffer):
			return 0

		return ord(buffer[pos]) + ord(buffer[pos+1]) * 256
	

	def LongAt(self,buffer,pos):

		if pos + 3 >= len(buffer):
			return 0
				
		return self.WordAt(buffer,pos) + self.WordAt(buffer,pos+2) * 256 * 256
			

	def ReadBlockID(self):
		
		id = self.WordAt(self.buffer, self.pos)
		self.pos = self.pos + 2
		return id
		

	def FindTapeBlock(self):
		
		id = -1

		while self.pos < len(self.buffer):
			
			id = self.ReadBlockID()
			length = self.LongAt(self.buffer, self.pos)
			
			if id in self.tapeIDList:
				break
				
			self.pos = self.pos + 4 + length

		return id in self.tapeIDList, id
	

	def ToBitString(self,buffer):
		
		power2 = [1,2,4,8,16,32,64,128]

		s = ""

		pos = 0
		while pos < len(buffer):

			s = s + self.startBit

			for i in range(0,8):
				if (ord(buffer[pos]) & power2[i]):
					s = s + "1"
				else:
					s = s + "0"

			s = s + self.stopBit
			
			pos = pos + 1

		return s


	def ReadTapeBlock(self,id):
		
		length   = self.LongAt(self.buffer,self.pos)
		self.pos = self.pos + 4
	
		if id == self.tapeID:
			ret = self.ToBitString(self.buffer[self.pos:self.pos + length])
		elif id == self.gapID:
			ms  = self.WordAt(self.buffer,self.pos)
			# HACK: Write gaps as 0
#			ret = int(ms * (BAUD/1000.0)) * " "
			ret = int(ms * (BAUD/1000.0)) * "0"
		elif id == self.highToneID:
			ms  = self.WordAt(self.buffer,self.pos)
			ret = int(ms * (BAUD/1000.0)) * "1"			
		
		self.pos = self.pos + length
		
		return ret

	
	def ReadAllTapeBlocks(self):
		
		ret = ""
		
		found, id = self.FindTapeBlock()
		while found:
			ret       = ret + self.ReadTapeBlock(id)
			found, id = self.FindTapeBlock()
		
		return ret


class Convertor:


	inFile  = ""
	outFile = ""

	volumeMin = 0
	volumeMax = 0


	def __init__(self,inFile,outFile):

		self.inFile  = inFile
		self.outFile = outFile
		
	
	def SetVolume(self,volume):
	
		self.volumeMax = volume


	def SetVolumeRange(self,min,max):
		
		self.volumeMin = min
		self.volumeMax = max


	def Convert(self):

		try:
			fpIn = open(self.inFile, "rb")
		except IOError:
			raise IOError, "Couldn't open file: " + self.inFile
		
		if fpIn.read(10) != "UEF File!\000":
			fpIn.close()

			try:
				fpIn = gzip.open(self.inFile, "rb")
				if fpIn.read(10) != "UEF File!\000":
					fpIn.close()
					raise IOError, "Not a UEF file: " + self.inFile

			except IOError:
				raise IOError, "Not a UEF file: " + self.inFile

		fpIn.read(2)
		buffer = fpIn.read()
		fpIn.close()
		
		try:
			fpOut = open(self.outFile, "wb")
		except IOError:
			raise IOError, "Couldn't open file: " + self.outFile
		
		parser    = UEFParser(buffer)
		bitStream = parser.ReadAllTapeBlocks()

		i = 0
		while i < len(bitStream) - 1:
			# output a copy/paste vhdl test
			# print(" when "+ str(i/8) +" => data <= \"" + bitStream[i:i+8] + "\";")			
			val = int(bitStream[i:i+8],2)
			fpOut.write(chr(val))
			i += 8
		
		fpOut.close()


# main

if sys.platform == "mac":

	# Mac has no command-line (pre OS X)
	
	fsSpecIn, ok = macfs.StandardGetFile()
	if not ok: sys.exit(1)
	
	fsSpecOut, ok = macfs.StandardPutFile("Save raw file as:")
	if not ok: sys.exit(1)
	
	convertor = Convertor(fsSpecIn.as_pathname(), fsSpecOut.as_pathname())
	
else:

	# a command-line is assumed here
	
	if len(sys.argv) != 3:
		print "Syntax:", sys.argv[0], "<inputfile.uef> <outputfile.raw>"
		sys.exit(1)
	
	convertor = Convertor(sys.argv[1], sys.argv[2])
	
convertor.SetVolumeRange(0x30, 0xC0)

try:
	convertor.Convert()
	print "All done."
except IOError, msg:
	print "An error occured:", msg
	sys.exit(1)

sys.exit()
