#ifndef _PERFQUERY_H_
#define _PERFQUERY_H_

#include "PerfQueryBase.h"

namespace OGL {

class PerfQuery : public PerfQueryBase
{
public:
	PerfQuery();
	~PerfQuery();

	void EnableQuery(PerfQueryGroup type);
	void DisableQuery(PerfQueryGroup type);
	void ResetQuery();
	u32 GetQueryResult(PerfQueryType type);
	void FlushResults();
	bool IsFlushed() const;

private:
	struct ActiveQuery
	{
		GLuint query_id;
		PerfQueryGroup query_type;
	};

	// when testing in SMS: 64 was too small, 128 was ok
	static const u32 PERF_QUERY_BUFFER_SIZE = 512;

	void WeakFlush();
	// Only use when non-empty
	void FlushOne();

	// This contains gl query objects with unretrieved results.
	ActiveQuery m_query_buffer[PERF_QUERY_BUFFER_SIZE];
	u32 m_query_read_pos;

	// TODO: sloppy
	volatile u32 m_query_count;
	volatile u32 m_results[PQG_NUM_MEMBERS];
};

} // namespace

#endif // _PERFQUERY_H_
