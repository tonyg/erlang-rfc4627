JsonRpcRequestId = 1;

JsonRpcTransaction = Class.create();
Object.extend(JsonRpcTransaction.prototype,
{
    initialize: function(serviceUrl, methodName, params, options) {
	this.options = {
	    asynchronous: true,
	    debug: false
	};
	Object.extend(this.options, options || {});
	this.serviceUrl = serviceUrl;
	this.methodName = methodName;
	this.params = params;
	this.error = null;
	this.reply = null;
	this.replyReady = 0;
	this.callbacks = [];
	this.errorCallbacks = [];
	this.sendRequest();
    },

    buildRequest: function() {
	return { version: "1.1",
		 id: JsonRpcRequestId++,
		 method: this.methodName,
		 params: this.params };
    },

    sendRequest: function() {
	this.request =
	    new Ajax.Request(this.serviceUrl,
			     { method: 'post',
			       requestHeaders: ['Content-type', 'application/json',
						'Accept', 'application/json'],
			       postBody: JSON.stringify(this.buildRequest()),
			       asynchronous: this.options.asynchronous,
			       onComplete: this.receiveReply.bind(this) });
	if (!this.options.asynchronous) {
	    this.receiveReply(this.request.transport);
	}
    },

    receiveReply: function(ajaxRequest) {
	var response = JSON.parse(ajaxRequest.responseText);
	if (response.error) {
	    if (this.options.debug) {
		alert("JsonRPC error:\n" +
		      "Service: " + JSON.stringify(this.serviceUrl) + "\n" +
		      "Method: " + JSON.stringify(this.methodName) + "\n" +
		      "Params: " + JSON.stringify(this.params) + "\n" +
		      "Response: " + JSON.stringify(response) + "\n");
	    }
	    this.error = response.error;
	    this.errorCallbacks.each(function (cb) {
					 try { cb(response.error, true); }
					 catch (err) {}
				     });
	} else {
	    var reply = response.result;
	    this.reply = reply;
	    this.replyReady = 1;
	    this.callbacks.each(function (cb) {
				    try { cb(reply, false); }
				    catch (err) {}
				});
	}
    },

    addCallback: function(cb) {
	this.callbacks.push(cb);
	if (this.replyReady) {
	    try { cb(this.reply, false); }
	    catch (err) {}
	}
	return this;
    },

    addErrorCallback: function(cb) {
	this.errorCallbacks.push(cb);
	if (this.error) {
	    try { cb(this.error, true); }
	    catch (err) {}
	}
	return this;
    }
});

JsonRpcService = Class.create();
Object.extend(JsonRpcService.prototype,
{
    initialize: function(serviceUrl, options) {
	this.options = {
	    transactionClass: JsonRpcTransaction,
	    debug: false
	};
	Object.extend(this.options, options || {});
	this.serviceUrl = serviceUrl;
	var txn = new (this.options.transactionClass)(serviceUrl,
						      "system.describe",
						      [],
						      {asynchronous: false,
						       debug: this.options.debug});
	this.serviceDescription = txn.reply;
	var svc = this;
	this.serviceDescription.procs.each(function (desc) {
					       svc[desc.name] = svc.makeGenericProxy(desc);
					   });
    },

    makeGenericProxy: function(desc) {
	return function () {
	    var actuals = $A(arguments);
	    return new (this.options.transactionClass)(this.serviceDescription.address,
						       desc.name,
						       actuals,
						       {debug: this.options.debug});
	};
    }
});
