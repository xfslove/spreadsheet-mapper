package me.excel.tools.transfer;

import java.io.InputStream;

/**
 * file transfer
 *
 * Created by hanwen on 15-12-16.
 */
public interface UserFileTransfer {

  /**
   * transfer inputStream to java bean
   *
   * @param inputStream
   */
  void transfer(InputStream inputStream);

}
