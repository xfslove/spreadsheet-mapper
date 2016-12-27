package me.excel.tools.processor;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * customer data processor
 * <p>
 * Created by hanwen on 16-1-18.
 */
public class CommonDataProcessorListener<D> implements DataProcessorListener {

  private Consumer<D> handleAcceptor;

  public CommonDataProcessorListener(Consumer<D> handleAcceptor) {
    this.handleAcceptor = handleAcceptor;
  }

  @Override
  public void beforeRow(Object model) {
    // nothing
  }

  @Override
  public void afterRow(Object model) {
    // nothing
  }

  @Override
  public void afterSheet(List models) {
    Optional<Consumer<D>> handleAcceptor = Optional.ofNullable(this.handleAcceptor);
    handleAcceptor.ifPresent(dConsumer -> models.forEach(model -> dConsumer.accept((D) model)));
  }
}
