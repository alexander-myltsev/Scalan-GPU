#ifndef NESTED_ARRAY_H
#define NESTED_ARRAY_H

namespace scalan_thrust {
  template <class T>
  class nested_array : public parray<parray<T>> {
  private:
    parray<T>& m_values;
    parray<int>& m_segments;
  public:
    nested_array() : m_values(base_array<T>()), m_segments(base_array<int>()) { }    
    nested_array(parray<T>& values, parray<int>& segments) : m_values(values), m_segments(segments) { }

    nested_array<T> &operator=(const nested_array<T>& ptr) {
      m_values = ptr.m_values; 
      m_segments = ptr.segments;
      return *this; 
    }

    nested_array(nested_array<T>* ptr) { 
      if (ptr != NULL) { 
        m_values = ptr->m_values;
        m_segments = ptr->m_segments;
      }
    }

    nested_array(const nested_array<T>& na) : m_values(na.m_values), m_segments(na.m_segments) { }

    ~nested_array() { }

    parray<int>& segments() const { return m_segments; }
    parray<T>& values() const { return m_values; }
    virtual int length() const { return segments().length(); }

    virtual parray<T>& get(int idx) const { 
      host_vector<int> idxs_hv(1, idx);
      base_array<int> idxs(idxs_hv);
      nested_array<T> na = dynamic_cast<nested_array<T>&>(this->back_permute(idxs));
      return na.m_values;
    }

    //base_array<T> map(const unary_operation<T>& op) {
    //parray<T> map(const unary_operation<T>& op) {
    //  return m_values;
    //}

    virtual parray<parray<T>>& back_permute(const parray<int>& idxs) const;

    virtual void print() const {
      printf("nested_array: [\n");
      printf("values-> ");
      values().print();
      printf("segments-> ");
      segments().print();
      printf("]\n");
    }

    virtual parray<parray<T>>& scan() const {
      return nested_array<T>();
    }

    virtual const parray<T>& sum() const {
      return base_array<T>();
    }
  }; // class nested_array<T>

  template <class T1, class T2>
  class nested_array <pair<T1, T2> > {
  private:
    pair_array<T1, T2>* m_values;
    parray<int>& m_segments;
  public:
    nested_array() : m_values(), m_segments() { }    
    nested_array(pair_array<T1, T2>* values, parray<int>& segments) : m_segments(segments) { 
      m_values = values; // TODO: Why polymorphism doesn't work for 'm_values = (const parray<T>& values)'?
    }

    parray<int>& segments() const { return m_segments; }
    pair_array<T1, T2>& values() const { return *m_values; }
    virtual int length() const { return segments().length(); }
  };


  // NOTE: nested_array.back_permute in terms of base_array.back_permute, i.e. no tricky bytes manipultaions
  //  val data = fromArray(Array(100, 101, 102, 103, 104, 105, 106, 107, 108, 109))
  //  val segsLens = fromArray(Array(4, 1, 0, 5))
  //  val segsIdxs = fromArray(Array(0, 4, 5, 5))
  //  val na = mkNestedArray(data, segsIdxs zip segsLens)
  //  val idxs = fromArray(Array(1, 3, 2, 0))
  //  val a1 = segsIdxs.backPermute(idxs)
  //  val a2 = segsLens.backPermute(idxs).scan
  //  val a3 = (a1 zip a2) map {case Pair(x, y) => x - y} // a3 = a1 |-| a2
  //  val idxs1Data = fromArray(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  //  val idxs1Lens = segsLens.backPermute(idxs)
  //  val idxs1Idxs = idxs1Lens.scan
  //  val idxs1 = mkNestedArray(idxs1Data, idxs1Idxs zip idxs1Lens)
  //  val a4 = a3.expandBy(idxs1)
  //  val a5 = (idxs1Data zip a4) map {case Pair(x, y) => x + y} // a5 = idxs1Data |+| a4
  //  val res = data.backPermute(a5)
  template <class T>
  parray<parray<T>>& nested_array<T>::back_permute(const parray<int>& idxs) const { // NOTE: Can idxs be not base_array but PA?
    // NOTE: generally idxs.length() != this.length()

    // this: [[a,b,c],[],[d],[e,f]]
    // values: [a,b,c,d,e,f]
    // segs: [3,0,1,2]
    // idxs: [3,0,1,2]
    // res: [[e,f],[a,b,c],[],[d]]

    std::cout << "idxs: " << idxs;
    parray<int>& segments_idxs = m_segments.scan();
    std::cout << "segments_idxs: " << segments_idxs;
    parray<int>& segments_idxs_permuted = segments_idxs.back_permute(idxs);
    std::cout << "segments_idxs_permuted: " << segments_idxs_permuted;
    parray<int>& segments_lens_permuted = m_segments.back_permute(idxs);
    std::cout << "segments_lens_permuted: " << segments_lens_permuted;
    parray<int>& segments_permuted_idxs = segments_lens_permuted.scan();
    base_array<int> segments_movement_diff = binop_array_sub(segments_idxs_permuted, segments_permuted_idxs);
    std::cout << "segments_movement_diff: " << segments_movement_diff;

    int len = segments_lens_permuted.sum();
    parray<int>& idxs_new_data = series(len);
    std::cout << "idxs_new_data: " << idxs_new_data;
    nested_array<int> idxs_new(idxs_new_data, segments_lens_permuted);
    std::cout << "idxs_new: " << idxs_new;
    base_array<int> a4 = segments_movement_diff.expand_by(idxs_new);
    std::cout << "a4: " << a4;
    base_array<int> a5 = binop_array_add(idxs_new_data, a4);
    std::cout << "a5: " << a5;
    parray<T>& res = m_values.back_permute(a5);
    std::cout << "res: " << res;

    // TODO: Fix this memory leak
    nested_array<T>* na = new nested_array<T>(res, segments_lens_permuted);
    return *na;
  }
}

#endif NESTED_ARRAY_H