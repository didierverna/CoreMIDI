#import <CoreMIDI/CoreMIDI.h>

#import <pthread.h>


pthread_mutex_t packets = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t packet_ready = PTHREAD_COND_INITIALIZER;
pthread_cond_t packet_handled = PTHREAD_COND_INITIALIZER;

int packetFlag = 0;
MIDIPacket* packet = NULL;
MIDIEndpointRef packetEndpoint = 0;

void handlePackets
(MIDIPacketList *pktlist, void *readProcRefCon, void *srcConnRefCon)
{
  pthread_mutex_lock (&packets);

  int packetsNumber = pktlist->numPackets;

  packet = &pktlist->packet[0];
  packetEndpoint = (MIDIEndpointRef) srcConnRefCon;

  int i = 0;
  while (i < packetsNumber)
    {
      if (! packetFlag) packetFlag++;

      pthread_cond_signal (&packet_ready);
      pthread_cond_wait (&packet_handled, &packets);

      if (! packetFlag)
	{
	  packet = MIDIPacketNext (packet);
	  i++;
	}
    }

  pthread_mutex_unlock (&packets);
}
